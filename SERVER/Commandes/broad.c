/*
** EPITECH PROJECT, 2025
** rffr
** File description:
** ffrr
*/

#include "../include/my.h"
#include <math.h>

int calculate_direction(player_t *sender, player_t *receiver, game_t game)
{
    int dx = receiver->x - sender->x;
    int dy = receiver->y - sender->y;

    if (dx > game.width / 2)
        dx -= game.width;
    if (dx < -game.width / 2)
        dx += game.width;
    if (dy > game.height / 2)
        dy -= game.height;
    if (dy < -game.height / 2)
        dy += game.height;

    float angle = atan2f((float)-dy, (float)dx) * 180 / M_PI;
    if (angle < 0)
        angle += 360;
    int base_dir;
    if (angle >= 337.5 || angle < 22.5)
        base_dir = 3;
    else if (angle < 67.5)
        base_dir = 2;
    else if (angle < 112.5)
        base_dir = 1;
    else if (angle < 157.5)
        base_dir = 8;
    else if (angle < 202.5)
        base_dir = 7;
    else if (angle < 247.5)
        base_dir = 6;
    else if (angle < 292.5)
        base_dir = 5;
    else
        base_dir = 4;
    int rotation[4][9] = {
        {0, 1, 2, 3, 4, 5, 6, 7, 8},
        {0, 3, 4, 5, 6, 7, 8, 1, 2},
        {0, 5, 6, 7, 8, 1, 2, 3, 4},
        {0, 7, 8, 1, 2, 3, 4, 5, 6}
    };
    return rotation[sender->direction][base_dir];
}

void broadcast_cmd(int client_fd, int i, char *buffer, info_t *info)
{
    player_t *sender = &info->game.players[i];

    if (!buffer || strncmp(buffer, "Broadcast ", 9) != 0 || strlen(buffer) <= 9) {
        writing(client_fd, "ko\n");
        return;
    }
    char *msg = buffer + 9;

    for (int j = 0; j < CLIENTS; j++) {
        if (!info->valid[j] || j == i)
            continue;
        player_t *receiver = &info->game.players[j];
        int direction = calculate_direction(sender, receiver, info->game);

        printf("[Broadcast] Envoi à client fd %d (joueur %d), direction %d, msg: %s\n",
               info->data_socket[j], j, direction, msg);

        int ret = dprintf(info->data_socket[j], "message %d, %s\n", direction, msg);
        if (ret < 0) {
            perror("Erreur écriture broadcast");
        }
    }
    writing(client_fd, "ok\n");
    dprintf(1, "pbc %d %s\n", sender->id, msg);
}
