/*
** EPITECH PROJECT, 2024
** B-YEP-400-COT-4-1-zappy-evans.zime [WSL: Ubuntu]
** File description:
** left.c
*/

#include "../include/my.h"

void left_cmd(int client_fd, int i, char *buffer, info_t *info)
{
    player_t *player = &info->game.players[i];

    // FIX: Correctly calculate new direction to avoid negative results.
    // (0 - 1) % 4 would be -1, this ensures it wraps to 3.
    player->direction = (player->direction - 1 + 4) % 4;
    dprintf(client_fd, "ok\n");
}