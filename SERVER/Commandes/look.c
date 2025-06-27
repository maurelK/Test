/*
** EPITECH PROJECT, 2024
** B-YEP-400-COT-4-1-zappy-evans.zime [WSL: Ubuntu]
** File description:
** look.c
*/

#include "../include/my.h"

void look_cmd(int client_fd, int i, char *buffer, info_t *info)
{
    player_t *p = &info->game.players[i];
    game_t *game = &info->game;
    const char *res_names[] = {
        "food", "linemate", "deraumere", "sibur",
        "mendiane", "phiras", "thystame"
    };

    char result[8192] = "[";
    int vision = p->level;
    int width = game->width;
    int height = game->height;

    for (int dist = 0; dist <= vision; dist++) {
        for (int offset = -dist; offset <= dist; offset++) {
            int tx = p->x;
            int ty = p->y;

            if (p->direction == 0) {
                ty = (p->y - dist + height) % height;
                tx = (p->x + offset + width) % width;
            } else if (p->direction == 1) {
                tx = (p->x + dist) % width;
                ty = (p->y + offset + height) % height;
            } else if (p->direction == 2) {
                ty = (p->y + dist) % height;
                tx = (p->x - offset + width) % width;
            } else if (p->direction == 3) {
                tx = (p->x - dist + width) % width;
                ty = (p->y - offset + height) % height;
            }

            tile_t *tile = &game->map[ty][tx];
            if (strlen(result) > 1)
                strcat(result, ", ");
            strcat(result, "");

            for (int j = 0; j < tile->player_count; j++)
                strcat(result, "player ");

            for (int r = 0; r < 7; r++) {
                for (int k = 0; k < tile->resources[r]; k++) {
                    strcat(result, res_names[r]);
                    strcat(result, " ");
                }
            }

            int len = strlen(result);
            if (len > 0 && result[len - 1] == ' ')
                result[len - 1] = '\0';
        }
    }

    strcat(result, "]\n");
    dprintf(client_fd, "%s", result);
}
