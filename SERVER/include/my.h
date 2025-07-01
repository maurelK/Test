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

typedef struct command_node_s {
    char *command;
    char *buffer;
    long long exec_time; // absolute time in ms
    struct command_node_s *next;
} command_node_t;

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
    command_node_t *command_queue;
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
    int port;
    int max_clients;
} game_t;

typedef struct {
    int player_id;
    char *command;
    char *buffer;
    int remaining_time;
} pendingcmd_t;

typedef struct info {
    int to_close[CLIENTS];
    int valid[CLIENTS];
    char users[CLIENTS][50];
    struct sockaddr_in data_addr[CLIENTS];
    int data_socket[CLIENTS];
    game_t game;
    int is_gui[CLIENTS];
    pendingcmd_t pending_cmd[CLIENTS];
}info_t;

typedef struct {
    char *command;
    void (*function)(int, int, char *, info_t *);
    int exec_time;
} command_t;




typedef struct {
    int port;
    int width;
    int height;
    char *team_names[MAX_TEAMS];
    int team_count;
    int max_clients;
    int freq;
} args_t;

typedef struct tile_info_s {
    game_t *game;
    player_t *player;
    int dist;
    int offset;
} tile_info_t;

typedef struct coords_s {
    int tx;
    int ty;
} coords_t;

typedef struct eject_data_s {
    info_t *info;
    int *ejected;
} eject_data_t;
typedef struct coord_s {
    int x;
    int y;
} coord_t;

//int parse_args(int ac, char **av, args_t *args);
void print_usage(void);
int handle_game_arg2(char **argv, int *i, int argc, info_t *info);
int handle_game_arg1(char **argv, int *i, int argc, info_t *info);
int parse_single_arg(char **argv, int *i, int argc, info_t *info);
int parse_teams(char **argv, int *i, int argc, info_t *info);
tile_t **allocate_map(int width, int height);
void print_hex(const char *label, const char *str);
void clean_strings(char *str);
void add_cmd(int player_id, char *command, int freq, info_t *info);
void process_commands(info_t *info);
void dispatch_sources(game_t *game);
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
player_t *create_player(info_t *info, int socket, char *team_name);
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
void handle_resource_regeneration(game_t *game);
void execute_command(info_t *info, player_t *player, command_node_t *cmd);

void send_all_bct(info_t *info, int gui_fd);
void process_commands(info_t *info);
void decrease_life(info_t *info);
#endif
