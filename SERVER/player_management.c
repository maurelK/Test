/*
** EPITECH PROJECT, 2024
** B-YEP-400-COT-4-1-zappy-evans.zime [WSL: Ubuntu]
** File description:
** axel.c
*/

#include "include/my.h"

team_t *find_team(game_t *game, char *team_name)
{
    team_t *team;
    int i = 0;

    for (;i < game->team_count; i++) {
        if (strcmp(game->teams[i].name, team_name) == 0) {
            team = &game->teams[i];
            break;
        }
    }
    return team;
}

player_t *createPlayer(info_t *info, int socket, char *team_name)
{
    game_t *game = &info->game;
    team_t *team = find_team(game, team_name);
    player_t *player;
    tile_t *tile;
    int player_id = -1;
    int i = 0;

    if (!team || team->current_clients >= team->max_clients) {
        printf("team not found");
        return NULL;
    }
    for (; i < CLIENTS; i++) {
        if (info->valid[i] == 0) {
            player_id = i;
            break;
        }
    }
    if (player_id == -1) {
        printf("maximun players reached\n");
        return NULL;
    }
    player = &game->players[player_id];
    player->id = player_id;
    player->direction = rand() % 4;
    player->incantation_time = 0;
    player->is_incanting = 0;
    player->level = 1;
    player->life_units = 126; 
    player->x = rand() % game->width;
    player->y = rand() % game->height;
    player->team_id = team - game->teams;
    memset(player->inventory, 0, sizeof(player->inventory));
    player->inventory[0] = 10;
    tile = &game->map[player->y][player->x];
    tile->player_ids[tile->player_count++] = player_id;
    team->current_clients++;
    game->player_count++;
    info->valid[player_id] = 1;
    info->data_socket[player_id] = socket;
    strncpy(info->users[player_id], team_name, 49);
    return player;
}

void decrease_life(info_t *info)
{
    for (int i = 0; i < CLIENTS; i++) {
        if (!info->valid[i])
            continue;

        player_t *p = &info->game.players[i];

        if (p->life_units > 0) {
            p->life_units--;
            
            if (p->life_units == 0) {
                p->inventory[0]--;   
                
                if (p->inventory[0] >= 0) {
                    p->life_units = 126; 
                } else {
                    
                    int fd = info->data_socket[i];
                    dprintf(fd, "dead\n");
                    close(fd);
                    info->valid[i] = 0;
                    info->data_socket[i] = -1;
                    printf("Player %d died of starvation\n", i);
                }
            }
        }
    }
}

void broadcast_cmd(int client_fd, int i, char *buffer, info_t *info){};
void incantation_cmd(int client_fd, int i, char *buffer, info_t *info){};
