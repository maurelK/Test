/*
** EPITECH PROJECT, 2025
** rffr
** File description:
** ffrr
*/

#include "../include/my.h"
#include <math.h>

int j;
int ret;
char *msg;
player_t *sender;
player_t *receiver;
int direction;

void normalize_position_diff(int *dx, int *dy, int width, int height)
{
    if (*dx > width / 2)
        *dx -= width;
    if (*dx < - width / 2)
        *dx += width;
    if (*dy > height / 2)
        *dy -= height;
    if (*dy < - height / 2)
        *dy += height;
}

int get_base_direction(float angle)
{
    int i;
    float thresholds[] = {22.5, 67.5, 112.5, 157.5,
        202.5, 247.5, 292.5, 337.5};
    int directions[] = {3, 2, 1, 8, 7, 6, 5, 4, 3};

    if (angle < 0)
        angle += 360;
    for (i = 0; i < 8; i++) {
        if (angle < thresholds[i])
            return directions[i];
    }
    return directions[8];
}

int apply_rotation(int direction, int base_dir)
{
    int rotation[4][9] = {
        {0, 1, 2, 3, 4, 5, 6, 7, 8},
        {0, 3, 4, 5, 6, 7, 8, 1, 2},
        {0, 5, 6, 7, 8, 1, 2, 3, 4},
        {0, 7, 8, 1, 2, 3, 4, 5, 6}
    };

    return rotation[direction][base_dir];
}

int calculate_direction(player_t *sender, player_t *receiver, game_t game)
{
    int dx = receiver->x - sender->x;
    int dy = receiver->y - sender->y;
    float angle = atan2f((float) - dy, (float)dx) * 180.0f / M_PI;
    int base_dir = get_base_direction(angle);

    normalize_position_diff(&dx, &dy, game.width, game.height);
    return apply_rotation(sender->direction, base_dir);
}

void broadcast_cmd(int client_fd, int i, char *buffer, info_t *info)
{
    msg = buffer + 9;
    sender = &info->game.players[i];
    receiver = &info->game.players[j];
    direction = calculate_direction(sender, receiver, info->game);
    ret = dprintf(info->data_socket[j], "message %d, %s\n", direction, msg);
    if (!buffer || strncmp(buffer, "Broadcast ", 9) != 0
    || strlen(buffer) <= 9) {
        writing(client_fd, "ko\n");
        return;
    }
    for (j = 0; j < CLIENTS; j++) {
        if (!info->valid[j] || j == i)
            continue;
        printf("[Bc] Env à client fd %d (joueur %d), dtion %d, msg: %s\n",
        info->data_socket[j], j, direction, msg);
        if (ret < 0)
            perror("Erreur écriture broadcast");
    }
    writing(client_fd, "ok\n");
    dprintf(1, "pbc %d %s\n", sender->id, msg);
}
