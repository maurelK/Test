/*
** EPITECH PROJECT, 2025
** Socket creation, poll activity
** File description:
** Create socket, Adressing, poll(), ...
*/

#include "include/my.h"
#include <time.h>


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
    const char *welcome_msg = "WELCOME\n";

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
    printf("Sent WELCOME to new client fd %d\n", new_client);
    return new_client;
}

int read_message(int client_fd, char *buffer, int size)
{
    int valread = read(client_fd, buffer, size);
    if (valread < 0) {
        perror("ko\n");
        close(client_fd);
        return -1;
    }
    if (valread == 0) {
        printf("Client %d est déconnecté\n", client_fd);
        close(client_fd);
        return 0;
    }
    buffer[valread] = '\0';
    return valread;
}


int writing(int client_fd, const char *message)
{
    return write(client_fd, message, strlen(message));
}

void add_new_client(int new_client, struct pollfd *ufds, info_t *info)
{
    for (int i = 0; i < CLIENTS; i++) {
        if (ufds[i].fd == -1) {
            ufds[i].fd = new_client;
            ufds[i].events = POLLIN;
            info->valid[i] = 0;
            info->data_socket[i] = new_client;
            info->is_gui[i] = 0;
            memset(info->users[i], 0, sizeof(info->users[i]));

            printf("New client assigned to slot %d, fd %d\n", i, new_client);
            return;
        }
    }
    printf("Maximum client limit reached. Refusing fd %d\n", new_client);
    close(new_client);
}


void send_all_bct(info_t *info, int gui_fd)
{
    for (int y = 0; y < info->game.height; y++) {
        for (int x = 0; x < info->game.width; x++) {
            tile_t *tile = &info->game.map[y][x];
            dprintf(gui_fd, "bct %d %d %d %d %d %d %d %d %d\n",
                x, y,
                tile->resources[0],
                tile->resources[1],
                tile->resources[2],
                tile->resources[3],
                tile->resources[4],
                tile->resources[5],
                tile->resources[6]);
        }
    }
}

void send_all_tna(info_t *info, int gui_fd)
{
    for (int i = 0; i < info->game.team_count; i++) {
        dprintf(gui_fd, "tna %s\n", info->game.teams[i].name);
    }
}

void send_all_pnw(info_t *info, int gui_fd)
{
    for (int i = 0; i < CLIENTS; i++) {
        if (!info->valid[i])
            continue;
        player_t *p = &info->game.players[i];
        dprintf(gui_fd, "pnw #%d %d %d %d %d %s\n",
                p->id, p->x, p->y, p->direction + 1, p->level, info->game.teams[p->team_id].name);
    }
}

void clean_string(char *str) {
    size_t len = strlen(str);
    while (len > 0 && (str[len - 1] == '\r' || str[len - 1] == '\n' || str[len - 1] == ' ')) {
        str[--len] = '\0';
    }
}


void handle_existing_connection(struct pollfd *ufds, int i, info_t *info)
{
    char buffer[1024];
    int valread = read_message(ufds[i].fd, buffer, sizeof(buffer) - 1);
    if (valread < 0)
        return;

    if (valread == 0) {
        printf("Client %d est déconnecté (read returned 0)\n", ufds[i].fd);
        for (int j = 0; j < CLIENTS; j++) {
            if (info->data_socket[j] == ufds[i].fd) {
                info->to_close[j] = 1;
                return;
            }
        }
        close(ufds[i].fd);
        ufds[i].fd = -1;
        return;
    }

    buffer[valread] = '\0';
    clean_string(buffer);
    printf("Raw message from client fd=%d: '%s'\n", ufds[i].fd, buffer);

    int player_id = -1;
    for (int j = 0; j < CLIENTS; j++) {
        if (info->data_socket[j] == ufds[i].fd) {
            player_id = j;
            break;
        }
    }

    if (player_id == -1) {
        printf("Client %d non associé à aucun slot\n", ufds[i].fd);
        dprintf(ufds[i].fd, "ko\n");
        close(ufds[i].fd);
        ufds[i].fd = -1;
        return;
    }

    // === GUI client ===
    if (!info->valid[player_id] && strcmp(buffer, "GRAPHIC") == 0) {
        info->is_gui[player_id] = 1;
        info->valid[player_id] = 1;
        printf("Client %d identifié comme interface graphique.\n", ufds[i].fd);
        dprintf(ufds[i].fd, "msz %d %d\n", info->game.width, info->game.height);
        dprintf(ufds[i].fd, "sgt %d\n", info->game.freq);
        send_all_tna(info, ufds[i].fd);
        send_all_bct(info, ufds[i].fd);
        send_all_pnw(info, ufds[i].fd);
        return;
    }

    // === Authentification IA ===
    if (!info->valid[player_id] && info->users[player_id][0] == '\0') {
        printf("Authentification IA: tentative avec team='%s'\n", buffer);

        int team_index = -1;
        for (int t = 0; t < info->game.team_count; t++) {
            printf(" - Comparaison avec team[%d]='%s'\n", t, info->game.teams[t].name);
            if (strcmp(buffer, info->game.teams[t].name) == 0) {
                team_index = t;
                break;
            }
        }
        if (team_index == -1) {
            printf("ERREUR: nom d'équipe invalide '%s'\n", buffer);
            dprintf(ufds[i].fd, "ko\n");
            close(ufds[i].fd);
            ufds[i].fd = -1;
            info->data_socket[player_id] = -1;
            return;
        }

        strncpy(info->users[player_id], buffer, sizeof(info->users[player_id]) - 1);
        info->users[player_id][sizeof(info->users[player_id]) - 1] = '\0';

        player_t *p = create_player(info, ufds[i].fd, buffer);
        if (!p) {
            printf("ERREUR: échec de createPlayer pour fd=%d\n", ufds[i].fd);
            dprintf(ufds[i].fd, "ko\n");
            close(ufds[i].fd);
            ufds[i].fd = -1;
            info->data_socket[player_id] = -1;
            return;
        }

        info->valid[player_id] = 1;

        int team_id = info->game.players[player_id].team_id;
        int remaining_slots = info->game.teams[team_id].max_clients - info->game.teams[team_id].current_clients;
        dprintf(ufds[i].fd, "%d\n", remaining_slots);
        dprintf(ufds[i].fd, "%d %d\n", info->game.width, info->game.height);

        printf("Client %d connecté à l'équipe '%s' (reste %d slots)\n",
               ufds[i].fd, buffer, remaining_slots);
        return;
    }

    // === Commande normale ===
    if (info->valid[player_id]) {
        printf("Commande reçue du client %d (player %d): %s\n", ufds[i].fd, player_id, buffer);
        add_cmd(player_id, buffer, info->game.freq);
    } else {
        printf("Client %d: message inattendu avant auth: %s\n", ufds[i].fd, buffer);
        dprintf(ufds[i].fd, "ko\n");
    }
}




void handle_poll(struct pollfd *ufds, int server, info_t *info)
{
    int ret;

    ufds[0].fd = server;
    ufds[0].events = POLLIN;

    while (1) {
        ret = poll(ufds, CLIENTS, 100);
        if (ret < 0) {
            perror("poll");
            exit(EXIT_FAILURE);
        }
        if (ret > 0) {
            if (ufds[0].revents & POLLIN) {
                int new_client = new_connexion(server);
                if (new_client >= 0) {
                    add_new_client(new_client, ufds, info);
                }
            }
            for (int i = 1; i < CLIENTS; i++) {
                if (ufds[i].fd != -1 && (ufds[i].revents & POLLIN)) {
                    handle_existing_connection(ufds, i, info);
                }
            }
        }
        process_commands(info);
        decrease_life(info);
    }
}

void my_server(int port, char *path, info_t *info)
{
    int server;
    struct pollfd ufds[CLIENTS];

    server = create_socket();
    binder(server, port);
    for (int i = 0; i < CLIENTS; i++) {
        ufds[i].fd = -1; 
    }
    handle_poll(ufds, server, info);
}

/*void handle_poll(struct pollfd *ufds, int server, info_t *info)
{
    ufds[0].fd = server;
    ufds[0].events = POLLIN;
    while (1) {
        poll_react(ufds, server, info);
    }
}*/

int count_active_fds(struct pollfd *ufds)
{
    int nfds = 0;
    int i;

    for (i = 0; i < CLIENTS; i++) {
        if (ufds[i].fd != -1) {
            nfds++;
        }
    }
    return nfds;
}

void poll_react(struct pollfd *ufds, int server, info_t *info)
{
    int nfds = count_active_fds(ufds);
    int react = poll(ufds, nfds, -1);

    if (react < 0) {
        perror("poll failed");
        return;
    }
    poll_event(ufds, server, info);
}

void poll_event(struct pollfd *ufds, int server, info_t *info)
{
    for (int i = 0; i < CLIENTS; i++) {
        if (ufds[i].fd == -1) {
            continue;
        }
        if (ufds[i].revents == 0)
            continue;
        poll_event_handler(ufds, server, info, i);
    }
    evets(ufds, server, info);
}

void handle_new_connection(int server, struct pollfd *ufds, info_t *info)
{
    int new_client = new_connexion(server);

    printf("New connection accepted!\n");
    add_new_client(new_client, ufds, info);
}

/*void handle_existing_connection(struct pollfd *ufds, int i, info_t *info)
{
    poll_event1(ufds, i, info);
}*/

void poll_event_handler(struct pollfd *ufds, int server, info_t *info, int i)
{
    if (ufds[i].revents & POLLIN) {
        if (ufds[i].fd == server) {
            handle_new_connection(server, ufds, info);
        } else {
            handle_existing_connection(ufds, i, info);
        }
    }
}

void reset_slot(info_t *info, int i, struct pollfd *ufds)
{
    if (ufds[i].fd != -1)
        close(ufds[i].fd);
    ufds[i].fd = -1;
    info->valid[i] = 0;
    info->to_close[i] = 0;
    info->is_gui[i] = 0;
    info->data_socket[i] = -1;
    memset(info->users[i], 0, sizeof(info->users[i]));
    memset(&info->game.players[i], 0, sizeof(player_t));
}

void evets(struct pollfd *ufds, int server, info_t *info)
{
    for (int i = 0; i < CLIENTS; i++) {
if (info->to_close[i]) {
    player_t *player = &info->game.players[i];
    tile_t *tile = &info->game.map[player->y][player->x];
    for (int j = 0; j < tile->player_count; j++) {
        if (tile->player_ids[j] == i) {
            for (int k = j; k < tile->player_count - 1; k++)
                tile->player_ids[k] = tile->player_ids[k + 1];
            tile->player_count--;
            break;
        }
    }
    int team_id = player->team_id;
    if (team_id >= 0 && team_id < info->game.team_count &&
        info->game.teams[team_id].current_clients > 0)
        info->game.teams[team_id].current_clients--;

    reset_slot(info, i, ufds);
}
    }
}

/*void usage (void) 
{
    printf("USAGE: ./zappy_server -p port -x width -y height ");
    printf("-n name1 name2 ... -c clientsNb -f freq");
}*/

void ressouces_initiation(game_t *game_tools, int i, int j)
{
    for (int k = 0; k < 7; k++) {
        game_tools->map[i][j].resources[k] = 0;
    }
}

tile_t **map_initiation(game_t *game_tools)
{
    game_tools->map = malloc(sizeof(tile_t *) * game_tools->height);
    if (!game_tools->map)
        return NULL;

    for (int i = 0; i < game_tools->height; i++) {
        game_tools->map[i] = malloc(sizeof(tile_t) * game_tools->width);
        if (!game_tools->map[i])
            return NULL;

        for (int j = 0; j < game_tools->width; j++) {
            ressouces_initiation(game_tools, i, j);
            game_tools->map[i][j].player_count = 0;
            game_tools->map[i][j].egg_count = 0;
        }
    }
    return game_tools->map;
}


void dispatch_sources(game_t *game)
{
    float resources[7] = {0.5, 0.3, 0.15, 0.1, 0.1, 0.08, 0.05};
    int total_tiles = game->width * game->height;
    coord_t *coords = malloc(sizeof(coord_t) * total_tiles);

    if (!coords)
        return;

    srand(time(NULL));

    // Remplir coords[]
    int idx = 0;
    for (int y = 0; y < game->height; y++)
        for (int x = 0; x < game->width; x++)
            coords[idx++] = (coord_t){x, y};

    // Pour chaque ressource :
    for (int i = 0; i < 7; i++) {
        int base = total_tiles / 7;
        int extra = (int)(resources[i] * total_tiles) - base;

        // Shuffle pour base
        for (int s = total_tiles - 1; s > 0; s--) {
            int j = rand() % (s + 1);
            coord_t tmp = coords[s];
            coords[s] = coords[j];
            coords[j] = tmp;
        }

        // Placer au moins 1 sur base tuiles
        for (int k = 0; k < base; k++) {
            int x = coords[k].x;
            int y = coords[k].y;
            game->map[y][x].resources[i]++;
        }

        // Ajouter du surplus
        for (int e = 0; e < extra; e++) {
            int r = rand() % total_tiles;
            int x = coords[r].x;
            int y = coords[r].y;
            game->map[y][x].resources[i]++;
        }
    }

    // Debug
    printf("Resource distribution on map:\n");
    for (int y = 0; y < game->height; y++) {
        for (int x = 0; x < game->width; x++) {
            printf("Tile (%d,%d): ", x, y);
            for (int r = 0; r < 7; r++) {
                printf("%d ", game->map[y][x].resources[r]);
            }
            printf("\n");
        }
    }

    free(coords);
}
