/*
** EPITECH PROJECT, 2024
** B-YEP-400-COT-4-1-zappy-evans.zime
** File description:
** eject.c
*/

#include "../include/my.h"

static void move_target(tile_t *tile, int id, player_t *p,
    eject_data_t *data)
{
    game_t *g = &data->info->game;
    player_t *t = &g->players[id];
    tile_t *d_t;

    t->x = (t->x + (p->direction == 1) - (p->direction == 3) + g->width)
        % g->width;
    t->y = (t->y + (p->direction == 2) - (p->direction == 0) + g->height)
        % g->height;
    d_t = &g->map[t->y][t->x];
    d_t->player_ids[d_t->player_count] = id;
    d_t->player_count++;
    dprintf(data->info->data_socket[id], "eject\n");
    *(data->ejected) = 1;
}

static void eject(tile_t *tile, player_t *p, eject_data_t *data)
{
    int id;

    for (int j = 0; j < tile->player_count;) {
        id = tile->player_ids[j];
        if (id == p->id) {
            j++;
            continue;
        }
        for (int k = j; k < tile->player_count - 1; k++)
            tile->player_ids[k] = tile->player_ids[k + 1];
        tile->player_count--;
        move_target(tile, id, p, data);
    }
}

void eject_cmd(int client_fd, int i, char *buffer, info_t *info)
{
    int ejected = 0;
    eject_data_t data = {info, &ejected};
    player_t *p = &info->game.players[i];
    tile_t *tile = &info->game.map[p->y][p->x];

    eject(tile, p, &data);
    dprintf(client_fd, ejected ? "ok\n" : "ko\n");
}
