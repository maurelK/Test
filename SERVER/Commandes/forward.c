/*
** EPITECH PROJECT, 2024
** B-YEP-400-COT-4-1-zappy-evans.zime
** File description:
** forward.c
*/

#include "../include/my.h"

void shift_player_ids(tile_t *tile, int from)
{
    for (int k = from; k < tile->player_count - 1; k++)
        tile->player_ids[k] = tile->player_ids[k + 1];
    tile->player_count--;
}

void remove_player_from_tile(tile_t *tile, int i)
{
    for (int j = 0; j < tile->player_count; j++) {
        if (tile->player_ids[j] == i) {
            shift_player_ids(tile, j);
            break;
        }
    }
}

void update_player_position(player_t *p, game_t *game)
{
    if (p->direction == 0)
        p->y = (p->y - 1 + game->height) % game->height;
    if (p->direction == 1)
        p->x = (p->x + 1) % game->width;
    if (p->direction == 2)
        p->y = (p->y + 1) % game->height;
    if (p->direction == 3)
        p->x = (p->x - 1 + game->width) % game->width;
}

void forward_cmd(int client_fd, int i, char *buffer, info_t *info)
{
    player_t *p = &info->game.players[i];
    tile_t *old_tile = &info->game.map[p->y][p->x];
    tile_t *new_tile;
    int num;

    remove_player_from_tile(old_tile, i);
    update_player_position(p, &info->game);
    new_tile = &info->game.map[p->y][p->x];
    new_tile->player_ids[new_tile->player_count] = i;
    new_tile->player_count++;
    dprintf(client_fd, "ok\n");
}
