/*
** EPITECH PROJECT, 2024
** B-YEP-400-COT-4-1-zappy-evans.zime
** File description:
** look.c
*/

#include "../include/my.h"

static void append_tile_contents(char *result, tile_t *tile,
    const char **res_names)
{
    int len;

    for (int j = 0; j < tile->player_count; j++)
        strcat(result, "player ");
    for (int r = 0; r < 7; r++) {
        for (int k = 0; k < tile->resources[r]; k++) {
            strcat(result, res_names[r]);
            strcat(result, " ");
        }
    }
    len = strlen(result);
    if (len > 0 && result[len - 1] == ' ')
        result[len - 1] = '\0';
}

static coords_t get_tile_coords(player_t *p, int dist,
    int offset, game_t *game)
{
    coords_t c = {p->x, p->y};

    if (p->direction == 0) {
        c.ty = (p->y - dist + game->height) % game->height;
        c.tx = (p->x + offset + game->width) % game->width;
    }
    if (p->direction == 1) {
        c.tx = (p->x + dist) % game->width;
        c.ty = (p->y + offset + game->height) % game->height;
    }
    if (p->direction == 2) {
        c.ty = (p->y + dist) % game->height;
        c.tx = (p->x - offset + game->width) % game->width;
    }
    if (p->direction == 3) {
        c.tx = (p->x - dist + game->width) % game->width;
        c.ty = (p->y - offset + game->height) % game->height;
    }
    return c;
}

static void append_tile(char *result, tile_info_t *info)
{
    coords_t c = get_tile_coords(info->player, info->dist,
        info->offset, info->game);
    tile_t *tile = &info->game->map[c.ty][c.tx];

    if (strlen(result) > 1)
        strcat(result, ", ");
    strcat(result, "");
    append_tile_contents(result, tile, (const char *[7]){
        "food", "linemate", "deraumere", "sibur",
        "mendiane", "phiras", "thystame"
    });
}

void look_cmd(int client_fd, int i, char *buffer, info_t *info)
{
    player_t *p = &info->game.players[i];
    game_t *game = &info->game;
    char result[8192] = "[";
    tile_info_t tile_info = {.game = &info->game, .player = p};

    for (int dist = 0; dist <= p->level; dist++) {
        tile_info.dist = dist;
        for (int offset = -dist; offset <= dist; offset++) {
            tile_info.offset = offset;
            append_tile(result, &tile_info);
        }
    }
    strcat(result, "]\n");
    dprintf(client_fd, "%s", result);
}
