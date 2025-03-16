/*
** EPITECH PROJECT, 2025
** poll event
** File description:
** organizer of code
*/

#include "include/my.h"

void evets(struct pollfd *ufds, int server, info_t *info)
{
    for (int i = 0; i < CLIENTS; i++) {
        if (info->to_close[i]) {
            close(ufds[i].fd);
            ufds[i].fd = -1;
            memset(&info->users[i], 0, sizeof(info->users[i]));
            info->valid[i] = 0;
            info->to_close[i] = 0;
        }
    }
}

void handle_new_connection(int server, struct pollfd *ufds)
{
    int new_client = new_connexion(server);

    printf("New connection accepted!\n");
    add_new_client(new_client, ufds);
}

void handle_existing_connection(struct pollfd *ufds, int i, info_t *info)
{
    poll_event1(ufds, i, info);
}

void poll_event_handler(struct pollfd *ufds, int server, info_t *info, int i)
{
    if (ufds[i].revents & POLLIN) {
        if (ufds[i].fd == server) {
            handle_new_connection(server, ufds);
        } else {
            handle_existing_connection(ufds, i, info);
        }
    }
}

void poll_event(struct pollfd *ufds, int server, info_t *info)
{
    for (int i = 0; i < CLIENTS; i++) {
        if (ufds[i].revents == 0)
            continue;
        poll_event_handler(ufds, server, info, i);
    }
    evets(ufds, server, info);
}
