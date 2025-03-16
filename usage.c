/*
** EPITECH PROJECT, 2025
** Help implementation
** File description:
** -help
*/
#include "include/my.h"

void print_help(void)
{
    printf("USAGE: ./myftp port path\n");
    printf("       port: port number on which the server socket listens\n");
    printf("       path: path to the home directory for the Anonymous user\n");
}

void add_new_client(int new_client, struct pollfd *ufds)
{
    int j = 1;

    for (; j < CLIENTS; j++) {
        if (ufds[j].fd == -1) {
            ufds[j].fd = new_client;
            ufds[j].events = POLLIN;
            printf("A client connected!\n");
            return;
        }
    }
    close(new_client);
}
