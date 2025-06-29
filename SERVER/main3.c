/*
** EPITECH PROJECT, 2025
** iunoufe
** File description:
** zfz
*/

#include "include/my.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

int validate_arguments(info_t *info)
{
    if (info->game.port <= 0 ||
        info->game.width <= 0 || info->game.height <= 0 ||
        info->game.max_clients <= 0
        || info->game.freq <= 0 || info->game.team_count == 0) {
        print_usage();
        return 0;
    }
    return 1;
}

int parse_arguments(int argc, char **argv, info_t *info)
{
    int i = 1;

    while (i < argc) {
        if (!parse_single_arg(argv, &i, argc, info))
            return 0;
        i++;
    }
    return validate_arguments(info);
}

int main(int argc, char **argv)
{
    info_t info = {0};
    int j;

    if (!parse_arguments(argc, argv, &info))
        return 84;
    info.game.map = allocate_map(info.game.width, info.game.height);
    if (!info.game.map) {
        fprintf(stderr, "Erreur allocation map\n");
        return 84;
    }
    for (j = 0; j < info.game.team_count; j++) {
        info.game.teams[j].max_clients = info.game.max_clients;
        info.game.teams[j].current_clients = 0;
        info.game.teams[j].egg_count = 0;
    }
    dispatch_sources(&info.game);
    my_server(info.game.port, NULL, &info);
    return 0;
}
