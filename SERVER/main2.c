/*
** EPITECH PROJECT, 2025
** efe
** File description:
** rtrtrt
*/

#include "include/my.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

size_t c;

int parse_teams(char **argv, int *i, int argc, info_t *info)
{
    (*i)++;
    info->game.team_count = 0;
    printf("== Enregistrement des noms d'équipes ==\n");
    while (*i < argc && argv[*i][0] != '-' &&
        info->game.team_count < MAX_TEAMS) {
        strncpy(info->game.teams[info->game.team_count].name, argv[*i], 49);
        info->game.teams[info->game.team_count].name[49] = '\0';
        printf("TEAM %d: Nom d'équipe enregistré: ", info->game.team_count);
        for (c = 0; c <
            strlen(info->game.teams[info->game.team_count].name); c++)
            printf("[%02X]",
            (unsigned char)info->game.teams[info->game.team_count].name[c]);
        printf("  (str = '%s')\n",
            info->game.teams[info->game.team_count].name);
        info->game.team_count++;
        (*i)++;
    }
    (*i)--;
    return 1;
}

int parse_single_arg(char **argv, int *i, int argc, info_t *info)
{
    if (strcmp(argv[*i], "-n") == 0 && *i + 1 < argc)
        return parse_teams(argv, i, argc, info);
    if (handle_game_arg1(argv, i, argc, info))
        return 1;
    if (handle_game_arg2(argv, i, argc, info))
        return 1;
    print_usage();
    return 0;
}

int handle_game_arg1(char **argv, int *i, int argc, info_t *info)
{
    if (strcmp(argv[*i], "-p") == 0 && *i + 1 < argc) {
        (*i)++;
        info->game.port = atoi(argv[*i]);
        return 1;
    }
    if (strcmp(argv[*i], "-x") == 0 && *i + 1 < argc) {
        (*i)++;
        info->game.width = atoi(argv[*i]);
        return 1;
    }
    if (strcmp(argv[*i], "-y") == 0 && *i + 1 < argc) {
        (*i)++;
        info->game.height = atoi(argv[*i]);
        return 1;
    }
    return 0;
}

int handle_game_arg2(char **argv, int *i, int argc, info_t *info)
{
    if (strcmp(argv[*i], "-c") == 0 && *i + 1 < argc) {
        (*i)++;
        info->game.max_clients = atoi(argv[*i]);
        return 1;
    }
    if (strcmp(argv[*i], "-f") == 0 && *i + 1 < argc) {
        (*i)++;
        info->game.freq = atoi(argv[*i]);
        return 1;
    }
    return 0;
}
