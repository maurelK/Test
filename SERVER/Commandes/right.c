/*
** EPITECH PROJECT, 2024
** B-YEP-400-COT-4-1-zappy-evans.zime [WSL: Ubuntu]
** File description:
** right.c
*/

#include "../include/my.h"

void right_cmd(int client_fd, int i, char *buffer, info_t *info)
{
    player_t *player = &info->game.players[i];

    player->direction = (player->direction + 1) % 4;
    dprintf(client_fd, "ok\n");
}
