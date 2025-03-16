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
    struct sockaddr_in data_addr[CLIENTS];
    int data_socket[CLIENTS];
}info_t;

typedef struct {
    char *command;
    void (*function)(int, int, char *, info_t *);
} command_t;



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
void handle_user(int client_fd, int i, char *buffer, info_t *info);
void handle_quit(int client_fd, int i, char *buffer, info_t *info);
void handle_pass(int client_fd, int i, char *buffer, info_t *info);
void handle_poll(struct pollfd *ufds, int server, info_t *info);
int parse_ip_and_port(const char *input, struct sockaddr_in *addr);
void handle_port(int client_fd, int i, char *buffer, info_t *info);
void evets(struct pollfd *ufds, int server, info_t *info);
void handle_new_connection(int server, struct pollfd *ufds);
void handle_existing_connection(struct pollfd *ufds, int i, info_t *info);
void poll_event_handler(struct pollfd *ufds, int server, info_t *info, int i);
void poll_event(struct pollfd *ufds, int server, info_t *info);
#endif
