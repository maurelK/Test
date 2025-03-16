/*
** EPITECH PROJECT, 2025
** Socket_minimal func
** File description:
** socket
*/

#include "include/my.h"

int create_socket(void)
{
    int server;

    server = socket(AF_INET, SOCK_STREAM, 0);
    if (server < 0) {
        perror("socket failed");
        exit(-1);
    }
    return server;
}

void binder(int server, int port)
{
    struct sockaddr_in server_inf;
    size_t size = sizeof(server_inf);

    server_inf.sin_family = AF_INET;
    server_inf.sin_port = htons(port);
    server_inf.sin_addr.s_addr = INADDR_ANY;
    if (bind(server, (struct sockaddr *) &server_inf, size) < 0) {
        perror("bind");
        exit(-1);
    }
    if (listen(server, CLIENTS) < 0) {
        perror("Erreur listen");
        exit(-1);
    }
}

int new_connexion(int server)
{
    struct sockaddr_in client_addr;
    int new_client = 0;
    socklen_t addr_len = sizeof(client_addr);
    const char *welcome_msg = "220 Welcome to myFTP!\r\n";

    new_client = accept(server, (struct sockaddr *)&client_addr, &addr_len);
    if (new_client < 0) {
        perror("accept");
        return -1;
    }
    if (write(new_client, welcome_msg, strlen(welcome_msg)) < 0) {
        perror("write");
        close(new_client);
        return -1;
    }
    return new_client;
}

void handle_poll(struct pollfd *ufds, int server, info_t *info)
{
    int react = 0;

    ufds[0].fd = server;
    ufds[0].events = POLLIN;
    while (1) {
        react = poll(ufds, CLIENTS, -1);
        if (react < 0) {
            perror("poll failed");
            continue;
        }
        poll_event(ufds, server, info);
    }
}
