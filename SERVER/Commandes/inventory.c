/*
** EPITECH PROJECT, 2024
** B-YEP-400-COT-4-1-zappy-evans.zime [WSL: Ubuntu]
** File description:
** inventory.c
*/

#include "../include/my.h"

void inventory_cmd(int client_fd, int i, char *buffer, info_t *info)
{
    player_t *player = &info->game.players[i];
    char response[1024] = "[";
    const char *res_names[] = {
        "food", "linemate", "deraumere", "sibur",
        "mendiane", "phiras", "thystame"
    };

    for (int j = 0; j < 7; j++) {
        char temp[64];
        sprintf(temp, "%s %d", res_names[j], player->inventory[j]);
        strcat(response, temp);
        if (j < 6)
            strcat(response, ",");
    }
    strcat(response, "]\n");
    dprintf(client_fd, "%s", response);
}
