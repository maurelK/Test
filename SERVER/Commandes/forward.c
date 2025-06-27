/*
** EPITECH PROJECT, 2024
** B-YEP-400-COT-4-1-zappy-evans.zime [WSL: Ubuntu]
** File description:
** forward.c
*/

#include "../include/my.h"

void forward_cmd(int client_fd, int i, char *buffer, info_t *info)
{
    player_t *p = &info->game.players[i];
    tile_t *old_tile = &info->game.map[p->y][p->x];
    tile_t *new_tile;

    for (int j; j < old_tile->player_count; i++) {
        if (old_tile->player_ids[j] == i) {
            for (int k; k < old_tile->player_count - 1; k++)
                old_tile->player_ids[k] = old_tile->player_ids[k + 1];
            old_tile->player_count--;
            break;
        }
    }
    if (p->direction == 0)
        p->y = (p->y - 1 + info->game.height) % info->game.height;
    else if (p->direction == 1)
        p->x = (p->x + 1) % info->game.width;
    else if (p->direction == 2)
        p->y = (p->y + 1) % info->game.height;
    else if (p->direction == 3)
        p->y = (p->x - 1 + info->game.width) % info->game.width;
    new_tile = &info->game.map[p->y][p->x];
    new_tile->player_ids[new_tile->player_count++] = i;
    dprintf(client_fd, "ok\n");
}
