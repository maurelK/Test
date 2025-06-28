/*
** EPITECH PROJECT, 2025
** command
** File description:
** incantation
*/

#include "../include/my.h"
#include <stdio.h>
#include <stdbool.h>

const int INCANTATION_REQUIREMENTS[7][7] = {
    {1, 0, 0, 0, 0, 0, 0},
    {2, 1, 1, 1, 0, 0, 0},
    {2, 2, 0, 1, 0, 2, 0},
    {4, 1, 1, 2, 0, 1, 0},
    {4, 1, 2, 1, 3, 0, 0},
    {6, 1, 2, 3, 0, 1, 0},
    {6, 2, 2, 2, 2, 2, 1}
};

bool can_incant(tile_t *tile, int level, info_t *info)
{
    const int *req = INCANTATION_REQUIREMENTS[level - 1];
    int matching_players = 0;
    int i;
    int player_id;

    if (level < 1 || level > 7)
        return false;
    for (i = 0; i < tile->player_count; i++) {
        player_id = tile->player_ids[i];
        if (info->valid[player_id] &&
            info->game.players[player_id].level == level)
            matching_players++;
    }
    if (matching_players < req[0])
        return false;
    for (i = 0; i < 7; i++) {
        if (tile->resources[i] < req[i + 1])
            return false;
    }
    return true;
}

void consume_resources(tile_t *tile, int level)
{
    const int *req = INCANTATION_REQUIREMENTS[level - 1];
    int i;

    for (i = 0; i < 7; i++) {
        tile->resources[i] -= req[i + 1];
        if (tile->resources[i] < 0) {
            tile->resources[i] = 0;
        }
    }
}

void elevate_players(tile_t *tile, int level, info_t *info)
{
    const int required_players = INCANTATION_REQUIREMENTS[level - 1][0];
    int upgraded = 0;
    int i;
    int player_id;

    for (i = 0; i < tile->player_count && upgraded < required_players; i++) {
        player_id = tile->player_ids[i];
        if (info->valid[player_id] &&
            info->game.players[player_id].level == level) {
            info->game.players[player_id].level++;
            dprintf(info->data_socket[player_id], "Current level: %d\n",
            info->game.players[player_id].level);
            dprintf(1, "plv %d %d\n", player_id,
            info->game.players[player_id].level);
            upgraded++;
        }
    }
}

void incantation_cmd(int client_fd, int i, char *buffer, info_t *info)
{
    player_t *player = &info->game.players[i];
    tile_t *tile = &info->game.map[player->y][player->x];

    if (!can_incant(tile, player->level, info))
        return writing(client_fd, "ko\n");
    consume_resources(tile, player->level);
    elevate_players(tile, player->level, info);
    dprintf(client_fd, "Elevation underway\n");
    dprintf(1, "pie %d %d ok\n", player->x, player->y);
}
