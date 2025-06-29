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
    char *arg = buffer + 5; // "Take " is 5 chars
    if (!arg || *arg == '\0' || *arg == '\n' || *arg == '\r') {
        writing(client_fd, "ko\n");
        return;
    }
    char *res_name = strtok(arg, " \n\r");
    if (res_name == NULL) {
        writing(client_fd, "ko\n");
        return;
    }
    
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
    char *arg = buffer + 4;
    if (!arg || *arg == '\0' || *arg == '\n' || *arg == '\r') {
        writing(client_fd, "ko\n");
        return;
    }
    char *res_name = strtok(arg, " \n\r");
    if (res_name == NULL) {
        writing(client_fd, "ko\n");
        return;
    }

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
        printf("[SET] Player %d set %s at (%d,%d). New count: %d\n",
               i, res_name, player->x, player->y, tile->resources[res_index]);
    } else {
        writing(client_fd, "ko\n");
    }
}
