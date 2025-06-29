#include "include/my.h"
#include <stdlib.h>
#include <time.h>

static int resource_regen_counter = 0;

void regenerate_resources(game_t *game)
{
    const float densities[7] = {0.5, 0.3, 0.15, 0.1, 0.1, 0.08, 0.05};
    int total_tiles = game->width * game->height;
    coord_t *coords = malloc(sizeof(coord_t) * total_tiles);

    if (!coords)
        return;

    // Fill coords array with all tile coordinates
    int idx = 0;
    for (int y = 0; y < game->height; y++) {
        for (int x = 0; x < game->width; x++) {
            coords[idx++] = (coord_t){x, y};
        }
    }

    srand(time(NULL));

    // Shuffle coords array
    for (int i = total_tiles - 1; i > 0; i--) {
        int j = rand() % (i + 1);
        coord_t tmp = coords[i];
        coords[i] = coords[j];
        coords[j] = tmp;
    }

    // For each resource, ensure at least one on the map and distribute evenly
    for (int r = 0; r < 7; r++) {
        int total_resource = (int)(densities[r] * total_tiles);
        if (total_resource < 1)
            total_resource = 1;

        // Reset resource counts on map for this resource
        for (int y = 0; y < game->height; y++) {
            for (int x = 0; x < game->width; x++) {
                game->map[y][x].resources[r] = 0;
            }
        }

        // Place one resource on each tile up to total_resource
        for (int i = 0; i < total_resource; i++) {
            int x = coords[i].x;
            int y = coords[i].y;
            game->map[y][x].resources[r]++;
        }
    }

    free(coords);
}

void handle_resource_regeneration(game_t *game)
{
    resource_regen_counter++;
    if (resource_regen_counter >= 20) {
        regenerate_resources(game);
        resource_regen_counter = 0;
        printf("Resources regenerated on the map.\n");
    }
}

void handle_poll(struct pollfd *ufds, int server, info_t *info)
{
    int ret;
    static int tick_counter = 0;

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

        tick_counter++;
        if (tick_counter >= 20) {
            handle_resource_regeneration(&info->game);
            tick_counter = 0;
        }
    }
}
