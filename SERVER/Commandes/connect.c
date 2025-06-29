/*
** EPITECH PROJECT, 2024
** B-YEP-400-COT-4-1-zappy-evans.zime [WSL: Ubuntu]
** File description:
** connect.c
*/

#include "../include/my.h"

void connect_cmd(int client_fd, int i, char *buffer, info_t *info)
{
    player_t *player = &info->game.players[i];
    team_t *team = &info->game.teams[player->team_id];
    int remaining;

    remaining = team->max_clients - team->current_clients;
    dprintf(client_fd, "%d\n", remaining);
}
