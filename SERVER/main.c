/*
** EPITECH PROJECT, 2025
** eff
** File description:
** fefe
*/

#include "include/my.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>


void print_usage(void)
{
    printf("Usage: ./zappy_server -p port -x width -y");
    printf(" height -n team1 team2 ... -c clientsNb -f freq\n");
}

tile_t **allocate_map(int width, int height)
{
    tile_t **map = malloc(sizeof(tile_t *) * height);
    int i;
    int j;

    if (!map)
        return NULL;
    for (i = 0; i < height; i++) {
        map[i] = malloc(sizeof(tile_t) * width);
        if (!map[i])
            return NULL;
        for (j = 0; j < width; j++) {
            memset(map[i][j].resources, 0, sizeof(int) * 7);
            map[i][j].player_count = 0;
            map[i][j].egg_count = 0;
        }
    }
    return map;
}

void clean_strings(char *str)
{
    size_t len = strlen(str);

    while (len > 0 && (str[len - 1] == '\r' ||
        str[len - 1] == '\n' ||
        str[len - 1] == ' ')) {
        len = len - 1;
        str[len] = '\0';
    }
}

void print_hex(const char *label, const char *str)
{
    size_t i;

    printf("%s: ", label);
    for (i = 0; i < strlen(str); i++) {
        printf("[%02x]", (unsigned char)str[i]);
    }
    printf("  (str = '%s')\n", str);
}
