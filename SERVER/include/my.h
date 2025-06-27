/*
** EPITECH PROJECT, 2023
** my.h
** File description:
** epitech library
*/

#ifndef MY_H_
    #define MY_H_
    #define MAX_EGGS 100
    #define MAX_TEAMS 10

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
    #define CLIENTS 500

typedef struct player_s {
    int id;
    int x;
    int y;
    int direction;
    int level;
    int life_units;
    int inventory[7];
    int team_id;
    int is_incanting;
    int incantation_time;
} player_t;

typedef struct egg_s {
    int x;
    int y;
    int id;
    int team_id;
    int is_hatched;
    double hatch_time;
} egg_t;

typedef struct tile_s {
    int resources[7];
    int player_count;
    int player_ids[CLIENTS];
    int egg_count;
    int egg_ids[CLIENTS];
} tile_t;

typedef struct team_s {
    char name[50];
    int max_clients;
    int current_clients;
    int egg_count;
    egg_t eggs[CLIENTS];
} team_t;

typedef struct game_s {
    int width;
    int height;
    tile_t **map;
    player_t players[CLIENTS];
    int player_count;
    int egg_count;
    team_t teams[10];
    int team_count;
    int freq;
    int time_unit;
} game_t;

typedef struct info {
    int to_close[CLIENTS];
    int valid[CLIENTS];
    char users[CLIENTS][50];
    struct sockaddr_in data_addr[CLIENTS];
    int data_socket[CLIENTS];
    game_t game;
    int is_gui[CLIENTS];
}info_t;

typedef struct {
    char *command;
    void (*function)(int, int, char *, info_t *);
    int exec_time;
} command_t;

typedef struct {
    int player_id;
    char *command;
    char *buffer;
    int remaining_time;
} pendingcmd_t;

typedef struct {
    int port;
    int width;
    int height;
    char *team_names[MAX_TEAMS];
    int team_count;
    int max_clients;
    int freq;
} args_t;

//int parse_args(int ac, char **av, args_t *args);
void print_usage(void);
void add_cmd(int player_id, char *command, int freq);
void process_commands(info_t* info);
void dispatch_sources(game_t game);
int get_resource_index(const char *name);
void handle_take(int client_fd, int i, char *buffer, info_t *info);
void handle_set(int client_fd, int i, char *buffer, info_t *info);
void execute_fork(int client_fd, int i, char *buffer, info_t *info);
void spawn_player_from_egg(player_t *player, team_t *team, game_t *game);
void print_help(void);
int create_socket(void);
void binder(int server, int port);
int new_connexion(int server);
void add_new_client(int new_client, struct pollfd *ufds, info_t *info);
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
void handle_new_connection(int server, struct pollfd *ufds, info_t *info);
void handle_existing_connection(struct pollfd *ufds, int i, info_t *info);
void poll_event_handler(struct pollfd *ufds, int server, info_t *info, int i);
void poll_event(struct pollfd *ufds, int server, info_t *info);
void handle_dele(int client_fd, int i, char *buffer, info_t *info);
void handle_help(int client_fd, int i, char *buffer, info_t *info);
void handle_noop(int client_fd, int i, char *buffer, info_t *info);
int count_active_fds(struct pollfd *ufds);
void poll_react(struct pollfd *ufds, int server, info_t *info);
// Axel functions prototypes:

player_t *createPlayer(info_t *info, int socket, char *team_name);
void decrease_life(info_t *info);
void handle_take(int client_fd, int i, char *buffer, info_t *info);
void handle_set(int client_fd, int i, char *buffer, info_t *info);
void look_cmd(int client_fd, int i, char *buffer, info_t *info);
void inventory_cmd(int client_fd, int i, char *buffer, info_t *info);
void broadcast_cmd(int client_fd, int i, char *buffer, info_t *info);
void connect_cmd(int client_fd, int i, char *buffer, info_t *info);
void execute_fork(int client_fd, int i, char *buffer, info_t *info);
void eject_cmd(int client_fd, int i, char *buffer, info_t *info);
void incantation_cmd(int client_fd, int i, char *buffer, info_t *info);
void forward_cmd(int client_fd, int i, char *buffer, info_t *info);
void left_cmd(int client_fd, int i, char *buffer, info_t *info);
void right_cmd(int client_fd, int i, char *buffer, info_t *info);

#endif
