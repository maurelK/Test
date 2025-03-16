/*
** EPITECH PROJECT, 2023
** my.h
** File description:
** epitech library
*/

#ifndef MY_H_
    #define MY_H_

    #include <stdlib.h>
    #include <string.h>
    #include <unistd.h>
    #include <arpa/inet.h>
    #include <stdio.h>
    #include <stdlib.h>
    #include <string.h>
    #include <unistd.h>
    #include <arpa/inet.h>
    #include <sys/socket.h>
    #include <sys/poll.h>
    #include <netinet/in.h>
    #define CLIENTS 3500

typedef struct info {
    int to_close[CLIENTS];
    int valid[CLIENTS];
    char users[CLIENTS][50];
}info_t;

void print_help(void);
int create_socket(void);
void binder(int server, int port);
int new_connexion(int server);
void add_new_client(int new_client, struct pollfd *ufds);
int read_message(int client_fd, char *buffer, int size);
int writing(int client_fd, const char *message);
void poll_event(struct pollfd *ufds, int server, info_t *info);
void poll_event1(struct pollfd *ufds, int i, info_t *info);
void my_server(int port, char *path, info_t *info);
void handle_user(int client_fd, struct pollfd *ufds, int i, char *buffer, info_t *info);
void handle_quit(int client_fd, struct pollfd *ufds, int i);
void handle_pass(int client_fd, struct pollfd *ufds, int i, char *buffer, info_t *info);
void handle_poll(struct pollfd *ufds, int server, info_t *info);



#endif
