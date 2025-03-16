/*
** EPITECH PROJECT, 2025
** User commands QUIT....
** File description:
** PASS,QUIT
*/

#include "include/my.h"

void handle_quit(int client_fd, int i, char *buffer, info_t *info)
{
    writing(client_fd, "221 Service closing control connection.\r\n");
    printf("Client %d disconnected.\n", client_fd);
    close(client_fd);
    info->to_close[i] = 1;
}

void handle_user(int client_fd, int i, char *buffer, info_t *info)
{
    char *username = NULL;

    username = strtok(buffer + 5, " \r\n");
    if (username == NULL) {
        writing(client_fd, "501 Syntax error in parameters.\r\n");
        return;
    }
    strncpy(info->users[i], username, sizeof(info->users[i]) - 1);
    info->users[i][sizeof(info->users[i]) - 1] = '\0';
    writing(client_fd, "331 User name okay, need password.\r\n");
    if (strcmp(info->users[i], "Anonymous") == 0) {
        info->valid[i] = 1;
    } else {
        info->valid[i] = 0;
    }
}

void handle_pass(int client_fd, int i, char *buffer, info_t *info)
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
    }
}

int parse_ip_and_port(const char *input, struct sockaddr_in *addr)
{
    unsigned char ip[4];
    int p1;
    int p2;

    if (sscanf(input, "%hhu,%hhu,%hhu,%hhu,%d,%d",
    &ip[0], &ip[1], &ip[2], &ip[3], &p1, &p2) != 6) {
        return -84;
    }
    addr->sin_addr.s_addr = (ip[0] << 24) | (ip[1] << 16) |
    (ip[2] << 8) | ip[3];
    addr->sin_port = htons((p1 << 8) + p2);
    addr->sin_family = AF_INET;
    return 0;
}

void handle_port(int client_fd, int i, char *buffer, info_t *info)
{
    struct sockaddr_in client_addr;

    if (info->valid[i] != 1) {
        writing(client_fd, "530 Not logged in.\r\n");
        return;
    }
    if (parse_ip_and_port(buffer + 5, &client_addr) != 0) {
        writing(client_fd, "501 Invalid PORT command.\r\n");
        return;
    }
    memcpy(&info->data_addr[i], &client_addr, sizeof(client_addr));
    writing(client_fd, "200 Command okay.\r\n");
}
