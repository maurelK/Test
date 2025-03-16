/*
** EPITECH PROJECT, 2025
** server principal files
** File description:
** myftp
*/

#include "include/my.h"



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
    char *cmd = NULL;
    command_t commands[] = {{"QUIT", handle_quit}, {"USER", handle_user},
    {"PASS", handle_pass}, {"PORT", handle_port}, {NULL, NULL}
    };

    if (bytes_read > 0) {
        printf("Message received: %s\n", buffer);
        cmd = strtok(buffer, " \r\n");
        if (cmd == NULL)
            return;
        for (int j = 0; commands[j].command != NULL; j++) {
            if (strcmp(cmd, commands[j].command) == 0) {
                commands[j].function(ufds[i].fd, i, buffer, info);
                return;
            }
        }
        writing(ufds[i].fd, "500 Unknown command.\r\n");
    }
}
