/*
** EPITECH PROJECT, 2025
** eff
** File description:
** fefe
*/

#include "include/my.h"

void print_usage(void)
{
    printf("Usage: ./zappy_server -p port -x width -y height -n team1 team2 ... -c clientsNb -f freq\n");
}

tile_t **allocate_map(int width, int height)
{
    tile_t **map = malloc(sizeof(tile_t *) * height);
    if (!map) return NULL;

    for (int i = 0; i < height; i++) {
        map[i] = malloc(sizeof(tile_t) * width);
        if (!map[i]) return NULL;

        for (int j = 0; j < width; j++) {
            memset(map[i][j].resources, 0, sizeof(int) * 7);
            map[i][j].player_count = 0;
            map[i][j].egg_count = 0;
        }
    }
    return map;
}

// Fonction utilitaire pour nettoyer une chaîne
void clean_strings(char *str) {
    size_t len = strlen(str);
    while (len > 0 && (str[len - 1] == '\r' || str[len - 1] == '\n' || str[len - 1] == ' ')) {
        str[--len] = '\0';
    }
}

// Affiche chaque caractère d'une chaîne en hexadécimal (debug)
void print_hex(const char *label, const char *str) {
    printf("%s: ", label);
    for (size_t i = 0; i < strlen(str); i++) {
        printf("[%02x]", (unsigned char)str[i]);
    }
    printf("  (str = '%s')\n", str);
}

int main(int argc, char **argv)
{
    info_t info = {0};
    int port = 0, width = 0, height = 0, max_clients = 0, freq = 0;
    int i = 1;

    // Parsing
    while (i < argc) {
        if (strcmp(argv[i], "-p") == 0 && i + 1 < argc)
            port = atoi(argv[++i]);
        else if (strcmp(argv[i], "-x") == 0 && i + 1 < argc)
            width = atoi(argv[++i]);
        else if (strcmp(argv[i], "-y") == 0 && i + 1 < argc)
            height = atoi(argv[++i]);
else if (strcmp(argv[i], "-n") == 0 && i + 1 < argc) {
    i++;
    info.game.team_count = 0;
    printf("== Enregistrement des noms d'équipes ==\n");
    while (i < argc && argv[i][0] != '-' && info.game.team_count < MAX_TEAMS) {
        strncpy(info.game.teams[info.game.team_count].name, argv[i], 49);
        info.game.teams[info.game.team_count].name[49] = '\0'; // Assure null-termination

        // Affiche les caractères hexadécimaux du nom d’équipe
        printf("TEAM %d: Nom d'équipe enregistré: ", info.game.team_count);
        for (size_t c = 0; c < strlen(info.game.teams[info.game.team_count].name); c++)
            printf("[%02X]", (unsigned char)info.game.teams[info.game.team_count].name[c]);
        printf("  (str = '%s')\n", info.game.teams[info.game.team_count].name);

        info.game.team_count++;
        i++;
    }
    i--; // Corrige l'incrémentation principale de la boucle
}

 else if (strcmp(argv[i], "-c") == 0 && i + 1 < argc)
            max_clients = atoi(argv[++i]);
        else if (strcmp(argv[i], "-f") == 0 && i + 1 < argc)
            freq = atoi(argv[++i]);
        else {
            print_usage();
            return 84;
        }
        i++;
    }
    if (port <= 0 || width <= 0 || height <= 0 || max_clients <= 0 || freq <= 0 || info.game.team_count == 0) {
        print_usage();
        return 84;
    }
    info.game.width = width;
    info.game.height = height;
    info.game.freq = freq;
    info.game.map = allocate_map(width, height);
    if (!info.game.map) {
        fprintf(stderr, "Erreur allocation map\n");
        return 84;
    }
    for (int j = 0; j < info.game.team_count; j++) {
        info.game.teams[j].max_clients = max_clients;
        info.game.teams[j].current_clients = 0;
        info.game.teams[j].egg_count = 0;
    }
    dispatch_sources(info.game);
    my_server(port, NULL, &info);
    return 0;
}
