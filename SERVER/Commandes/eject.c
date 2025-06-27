/*
** EPITECH PROJECT, 2024
** B-YEP-400-COT-4-1-zappy-evans.zime [WSL: Ubuntu]
** File description:
** eject.c
*/

#include "../include/my.h"

void eject_cmd(int client_fd, int i, char *buffer, info_t *info)
{
    player_t *p = &info->game.players[i];
    player_t *target;
    game_t *game = &info->game;
    tile_t *tile = &game->map[p->y][p->x];
    tile_t *new_tile;
    int width = game->width;
    int height = game->height;
    int ejected = 0;

    for (int j = 0; j < tile->player_count;) {
        int target_id = tile->player_ids[j];

        if (target_id == i) {
            j++;
            continue;
        }
        target = &game->players[target_id];
        for (int k = j; k < tile->player_count - 1; k++)
            tile->player_ids[k] = tile->player_ids[k + 1];
        tile->player_count--;
        if (p->direction == 0) // NORD
            target->y = (target->y - 1 + height) % height;
        else if (p->direction == 1) // EST
            target->x = (target->x + 1) % width;
        else if (p->direction == 2) // SUD
            target->y = (target->y + 1) % height;
        else if (p->direction == 3) // OUEST
            target->x = (target->x - 1 + width) % width;
        new_tile = &game->map[target->y][target->x];
        new_tile->player_ids[new_tile->player_count++] = target_id;
        dprintf(info->data_socket[target_id], "eject\n");
        ejected = 1;
    }

    if (ejected)
        dprintf(client_fd, "ok\n");
    else
        dprintf(client_fd, "ko\n");
}
