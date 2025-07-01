/*
** EPITECH PROJECT, 2024
** B-YEP-400-COT-4-1-zappy-evans.zime [WSL: Ubuntu]
** File description:
** fork_eggs.c
*/

#include "include/my.h"

int tile_egg_index;
player_t *player;
tile_t *tile;
team_t *team;
egg_t *egg;
int egg_id;


int generate_egg_id(game_t *game)
{
    int id = game->egg_count;

    game->egg_count++;
    return id;
}

void execute_fork(int client_fd, int i, char *buffer, info_t *info)
{
    player = &info->game.players[i];
    tile = &info->game.map[player->y][player->x];
    team = &info->game.teams[player->team_id];
    egg = &team->eggs[team->egg_count];
    egg_id = team->egg_count;
    (void)buffer;
    if (team->egg_count >= CLIENTS || tile->egg_count >= CLIENTS)
        return;
    dprintf(1, "Player pos: x=%d y=%d\n", player->x, player->y);
    egg->x = player->x;
    egg->y = player->y;
    egg->team_id = player->team_id;
    egg->is_hatched = 0;
    egg->hatch_time = 42.0 / info->game.freq;
    tile_egg_index = tile->egg_count;
    tile->egg_ids[tile_egg_index] = egg_id;
    tile->egg_count++;
    team->egg_count++;
    dprintf(client_fd, "ok\n");
    dprintf(1, "enw %d %d %d %d\n", egg_id, player->id, egg->x, egg->y);
}

egg_t *get_available_egg(team_t *team)
{
    int i;

    for (i = 0; i < team->egg_count; i++) {
        if (!team->eggs[i].is_hatched) {
            team->eggs[i].is_hatched = 1;
            return &team->eggs[i];
        }
    }
    return NULL;
}

void spawn_player_from_egg(player_t *player, team_t *team, game_t *game)
{
    egg_t *egg = get_available_egg(team);

    if (egg) {
        player->x = egg->x;
        player->y = egg->y;
        dprintf(1, "ebo %d\n", game->egg_count);
        // Remove egg from tile
        tile_t *tile = &game->map[egg->y][egg->x];
        for (int i = 0; i < tile->egg_count; i++) {
            if (tile->egg_ids[i] == egg->id) {
                // Shift egg ids left
                for (int j = i; j < tile->egg_count - 1; j++) {
                    tile->egg_ids[j] = tile->egg_ids[j + 1];
                }
                tile->egg_count--;
                break;
            }
        }
        // Remove egg from team eggs array by marking as hatched
        egg->is_hatched = 1;
    } else {
        player->x = rand() % game->width;
        player->y = rand() % game->height;
    }
}
