/*
** EPITECH PROJECT, 2025
** Control Center
** File description:
** Will be the control center of the programm help me to monitor everything
*/


#include "include/my.h"


void my_server(int port, char *path, info_t *info)
{
    int server;
    struct pollfd ufds[CLIENTS];

    server = create_socket();
    binder(server, port);
    for (int i = 0; i < CLIENTS; i++) {
        ufds[i].fd = -1;
    }
    handle_poll(ufds, server, info);
}

int condition(char **av, char *path, int port, info_t *info)
{
    port = atoi(av[1]);
    if (port <= 0 || port > 65535) {
        free(info);
        return 84;
    }
    path = av[2];
    if (access(path, F_OK) == -1) {
        free(info);
        return 84;
    }
    my_server(port, path, info);
}

int main(int ac, char **av)
{
    int port = 0;
    char *path = NULL;
    info_t *info = NULL;
    int c = 0;

    if (ac == 2 && strcmp(av[1], "-help") == 0) {
        print_help();
        return 0;
    }
    if (ac != 3)
        return 84;
    info = malloc(sizeof(info_t));
    if (info == NULL)
        return 84;
    memset(info, 0, sizeof(info_t));
    c = condition(av, path, port, info);
    if (c == 84)
        return 84;
    free(info);
    return 0;
}
