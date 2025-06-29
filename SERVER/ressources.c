/*
** EPITECH PROJECT, 2025
** efef
** File description:
** feef
*/

#include "include/my.h"

const char *resource_names[7] = {
    "food", "linemate", "deraumere", "sibur", "mendiane", "phiras", "thystame"
};

int get_resource_index(const char *name)
{
    for (int i = 0; i < 7; i++) {
        if (strcmp(name, resource_names[i]) == 0)
            return i;
    }
    return -1;
}

void handle_take(int client_fd, int i, char *buffer, info_t *info)
{
    char *res_name = strtok(buffer + 5, "\n");
    int res_index = get_resource_index(res_name);
    player_t *player = &info->game.players[i];
    tile_t *tile = &info->game.map[player->y][player->x];

    if (res_index == -1) {
        writing(client_fd, "ko\n");
        return;
    }
    if (tile->resources[res_index] > 0) {
        tile->resources[res_index]--;
        player->inventory[res_index]++;
        writing(client_fd, "ok\n");
        printf("[TAKE] Player %d took %s at (%d,%d). New count: %d\n",
               i, res_name, player->x, player->y, tile->resources[res_index]);
    } else {
        writing(client_fd, "ko\n");
    }
}

void handle_set(int client_fd, int i, char *buffer, info_t *info)
{
    char *res_name = strtok(buffer + 4, "\n");
    int res_index = get_resource_index(res_name);
    player_t *player = &info->game.players[i];
    tile_t *tile = &info->game.map[player->y][player->x];

    if (res_index == -1) {
        writing(client_fd, "ko\n");
        return;
    }
    if (player->inventory[res_index] > 0) {
        player->inventory[res_index]--;
        tile->resources[res_index]++;
        writing(client_fd, "ok\n");
    } else {
        writing(client_fd, "ko\n");
    }
}
