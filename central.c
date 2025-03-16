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

int main(int ac, char **av)
{
    int port = 0;
    char *path = NULL;
    info_t *info = malloc(sizeof(info_t));
    //info->users = {0};

    if (ac == 2 && strcmp(av[1], "-help") == 0)
        print_help();
    if (ac != 3)
        return 84;
    port = atoi(av[1]);
    path = av[2];
    my_server(port, path, info);
}
