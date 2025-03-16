/*
** EPITECH PROJECT, 2025
** server principal files
** File description:
** myftp
*/

#include "include/my.h"


void add_new_client(int new_client, struct pollfd *ufds)
{
    int j = 1;

    for (; j < CLIENTS; j++) {
        if (ufds[j].fd == -1) {
            ufds[j].fd = new_client;
            ufds[j].events = POLLIN;
            printf("Le Client a été ajouté avec succès\n");
            return;
        }
    }
    close(new_client);
}

int read_message(int client_fd, char *buffer, int size)
{
    int valread = 0;

    valread = read(client_fd, buffer, size);
    if (valread < 0) {
        perror("Erreur lors de la lecture des données");
        close(client_fd);
        exit(-1);
    }
    if (valread == 0) {
        printf("Client %d est déconnecté\n", client_fd);
        close(client_fd);
        exit(-1);
    }
    buffer[valread] = '\0';
    return valread;
}

int writing(int client_fd, const char *message)
{
    return write(client_fd, message, strlen(message));
}

void poll_event1(struct pollfd *ufds, int i, info_t *info)
{
    char buffer[2000];

    int bytes_read = read_message(ufds[i].fd, buffer, sizeof(buffer));
    if (bytes_read > 0) {
        printf("Message received: %s\n", buffer);

        if (strncmp(buffer, "QUIT", 4) == 0) {
            handle_quit(ufds[i].fd, ufds, i);
            info->to_close[i] = 1;
        } else if (strncmp(buffer, "USER", 4) == 0) {
            handle_user(ufds[i].fd, ufds, i, buffer, info);
        } else if (strncmp(buffer, "PASS", 4) == 0) {
            handle_pass(ufds[i].fd, ufds, i, buffer, info);
        } else {
            info->to_close[i] = 1;
        }
    }
}

void poll_event(struct pollfd *ufds, int server, info_t *info)
{
    for (int i = 0; i < CLIENTS; i++) {
        if (ufds[i].revents == 0)
            continue;

        if (ufds[i].revents & POLLIN) {
            if (ufds[i].fd == server) {
                int new_client = new_connexion(server);
                printf("New connection accepted!\n");
                add_new_client(new_client, ufds);
            } else {
                poll_event1(ufds, i, info);
            }
        }
    }

    for (int i = 0; i < CLIENTS; i++) {
        if (info->to_close[i]) {
            close(ufds[i].fd);
            ufds[i].fd = -1;
        }
    }
}