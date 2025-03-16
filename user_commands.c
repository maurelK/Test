/*
** EPITECH PROJECT, 2025
** User commands QUIT....
** File description:
** PASS,QUIT
*/

#include "include/my.h"

void handle_quit(int client_fd, struct pollfd *ufds, int i)
{
    writing(client_fd, "221 Service closing control connection.\n");
    writing(client_fd, "    Logged out if appropriate.\n");
    printf("Client %d disconnected.\n", client_fd);
    close(client_fd);
    ufds[i].fd = -1;
}


void handle_user(int client_fd, struct pollfd *ufds, int i, char *buffer, info_t *info)
{
    char *pch = strtok(buffer, " ");
    pch = strtok(NULL, " \r\n");
    if (pch == NULL) {
        writing(client_fd, "501 Syntax error in parameters.\r\n");
        return;
    }
    strncpy(info->users[i], pch, sizeof(info->users[i]) - 1);
    info->users[i][sizeof(info->users[i]) - 1] = '\0';
    if (strcmp(info->users[i], "Anonymous") == 0) {
        writing(client_fd, "331 User name okay, need password.\r\n");
        info->valid[i] = 1;
    } else {
        writing(client_fd, "530 Not logged in.\r\n");
        info->users[i][0] = '\0';
    }
}

void handle_pass(int client_fd, struct pollfd *ufds, int i, char *buffer, info_t *info)
{
    if (info->users[i][0] == '\0') {
        writing(client_fd, "503 Login first.\r\n");
        return;
    }
    if (strcmp(info->users[i], "Anonymous") == 0) {
        writing(client_fd, "230 User logged in, proceed.\r\n");
        info->valid[i] = 1;
    } else {
        writing(client_fd, "530 Login incorrect.\r\n");
        info->users[i][0] = '\0';
    }
}