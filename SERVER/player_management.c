/*
** EPITECH PROJECT, 2024
** B-YEP-400-COT-4-1-zappy-evans.zime
** File description:
** axel.c
*/

#include "include/my.h"

team_t *find_team(game_t *game, char *team_name)
{
    for (int i = 0; i < game->team_count; i++) {
        if (strcmp(game->teams[i].name, team_name) == 0)
            return &game->teams[i];
    }
    return NULL;
}

static int get_available_player_id(info_t *info)
{
    for (int i = 0; i < CLIENTS; i++) {
        if (!info->valid[i])
            return i;
    }
    return -1;
}

static void init_player(player_t *p, info_t *info, int id, int socket)
{
    game_t *game = &info->game;

    p->id = id;
    p->direction = rand() % 4;
    p->incantation_time = 0;
    p->is_incanting = 0;
    p->level = 1;
    p->life_units = 1260; // Increased initial life units to 1260 for longer survival
    p->x = rand() % game->width;
    p->y = rand() % game->height;
    memset(p->inventory, 0, sizeof(p->inventory));
    p->inventory[0] = 20; // Increased initial food inventory to 20
    info->valid[id] = 1;
    info->data_socket[id] = socket;
}

static void set_player_team(player_t *p, team_t *team, game_t *game)
{
    p->team_id = team - game->teams;
}

static void place_player_on_map(game_t *game, int id, int x, int y)
{
    tile_t *tile = &game->map[y][x];

    tile->player_ids[tile->player_count] = id;
    tile->player_count++;
}

void finalise_creation(info_t *info, team_t *team,
    int player_id, char *team_name)
{
    team->current_clients++;
    info->game.player_count++;
    strncpy(info->users[player_id], team_name, 49);
    info->users[player_id][49] = '\0';
}

player_t *create_player(info_t *info, int socket, char *team_name)
{
    game_t *game = &info->game;
    team_t *team = find_team(game, team_name);
    player_t *p;
    int id;

    if (!team || team->current_clients >= team->max_clients) {
        printf("team not found\n");
        return NULL;
    }
    id = get_available_player_id(info);
    if (id == -1) {
        printf("maximum players reached\n");
        return NULL;
    }
    p = &game->players[id];
    init_player(p, info, id, socket);
    set_player_team(p, team, game);
    place_player_on_map(game, p->id, p->x, p->y);
    finalise_creation(info, team, id, team_name);
    return p;
}

static void kill_player(info_t *info, int i)
{
    int fd = info->data_socket[i];
    player_t *player = &info->game.players[i];
    tile_t *tile = &info->game.map[player->y][player->x];

    if (info->is_gui[i]) {
        // Do not kill GUI clients
        return;
    }

    dprintf(fd, "dead\n");
    close(fd);
    info->valid[i] = 0;
    info->data_socket[i] = -1;

    // Remove player from tile
    for (int j = 0; j < tile->player_count; j++) {
        if (tile->player_ids[j] == i) {
            for (int k = j; k < tile->player_count - 1; k++)
                tile->player_ids[k] = tile->player_ids[k + 1];
            tile->player_count--;
            break;
        }
    }

    printf("Player %d died of starvation\n", i);
}

void decrease_life(info_t *info)
{
    player_t *p;

    for (int i = 0; i < CLIENTS; i++) {
        if (!info->valid[i])
            continue;
        p = &info->game.players[i];
        if (p->life_units > 0) {
            p->life_units--;
            continue;
        }
        if (p->inventory[0] > 0) {
            p->inventory[0]--;
            p->life_units = 1260; // Reset life units to 1260 after consuming food
            printf("Player %d consumed food, life reset to 1260\n", i);
            continue;
        }
        kill_player(info, i);
    }
}
