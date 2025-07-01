#include "include/my.h"
#include <stdlib.h>
#include <time.h>
#include <time.h>
#include <poll.h>
#include <sys/time.h>


#ifndef CLOCK_MONOTONIC
#define CLOCK_MONOTONIC 1
#endif

#include "include/my.h"
#include <time.h>
#include <sys/time.h>

// Declare pending command variables globally as in handle_cmd.c
extern pendingcmd_t pending_cmd[CLIENTS * 10];
extern int pending_c;

// Function declarations


// Resource regeneration state
static int resource_regen_ticks = 0;

// Time management
long long get_current_time_ms() {
    struct timeval tv;
    gettimeofday(&tv, NULL);
    return (tv.tv_sec * 1000LL) + (tv.tv_usec / 1000LL);
}

// Resource regeneration function
void regenerate_resources(game_t *game)
{
    const float densities[7] = {0.5, 0.3, 0.15, 0.1, 0.1, 0.08, 0.05};
    int total_tiles = game->width * game->height;
    int *tile_indices = malloc(total_tiles * sizeof(int));
    
    if (!tile_indices) return;

    // Initialize tile indices
    for (int i = 0; i < total_tiles; i++)
        tile_indices[i] = i;

    // Fisher-Yates shuffle
    for (int i = total_tiles - 1; i > 0; i--) {
        int j = rand() % (i + 1);
        int tmp = tile_indices[i];
        tile_indices[i] = tile_indices[j];
        tile_indices[j] = tmp;
    }

    // Process all resources
    for (int r = 0; r < 7; r++) {
        int total = (int)(densities[r] * total_tiles);
        if (total < 1) total = 1;
        
        // Distribute resources
        for (int i = 0; i < total; i++) {
            int idx = tile_indices[i % total_tiles];
            int x = idx % game->width;
            int y = idx / game->width;
            game->map[y][x].resources[r]++;
        }
    }

    free(tile_indices);
}

// Process game logic each tick
void process_game_tick(info_t *info)
{
    // Process all commands
    process_commands(info);

    // Handle player life
    decrease_life(info);
    
    // Handle resource regeneration
    static int resource_regen_ticks = 0;
    if (++resource_regen_ticks >= 20) {
        regenerate_resources(&info->game);  // Changed to actual function
        resource_regen_ticks = 0;
        
        // Notify GUI clients
        for (int i = 0; i < CLIENTS; i++) {
            if (info->valid[i] && info->is_gui[i]) {
                send_all_bct(info, info->data_socket[i]);
            }
        }
    }
}

// Main event loop remains the same as previously provided
// Main event loop
void handle_poll(struct pollfd *ufds, int server, info_t *info)
{
    ufds[0].fd = server;
    ufds[0].events = POLLIN;
    long long last_tick_time = get_current_time_ms();
    
    while (1) {
        // Calculate time until next tick
        long long current_time = get_current_time_ms();
        long long tick_duration = 1000 / info->game.freq;  // ms per tick
        long long next_tick_time = last_tick_time + tick_duration;
        int timeout = (next_tick_time > current_time) ? 
                     (next_tick_time - current_time) : 0;

        // Prepare pollfd array
        int nfds = 0;
        struct pollfd active_ufds[CLIENTS];
        for (int i = 0; i < CLIENTS; i++) {
            if (ufds[i].fd != -1) {
                active_ufds[nfds] = ufds[i];
                nfds++;
            }
        }
        
        // Wait for events or next tick
        int ret = poll(active_ufds, nfds, timeout);
        if (ret < 0) {
            perror("poll");
            exit(EXIT_FAILURE);
        }

        // Handle I/O events
        if (ret > 0) {
            for (int i = 0; i < nfds; i++) {
                if (active_ufds[i].revents & POLLIN) {
                    if (active_ufds[i].fd == server) {
                        int new_client = new_connexion(server);
                        if (new_client >= 0) {
                            add_new_client(new_client, ufds, info);
                        }
                    } else {
                        // Find original index in ufds
                        for (int j = 0; j < CLIENTS; j++) {
                            if (ufds[j].fd == active_ufds[i].fd) {
                                handle_existing_connection(ufds, j, info);
                                break;
                            }
                        }
                    }
                }
            }
        }

        // Process game ticks based on elapsed time
        current_time = get_current_time_ms();
        int ticks_elapsed = 0;
        
        while (current_time - last_tick_time >= tick_duration) {
            ticks_elapsed++;
            last_tick_time += tick_duration;
            
            // Prevent catching up too many ticks
            if (ticks_elapsed > 5) {
                last_tick_time = current_time - tick_duration;
                break;
            }
        }

        // Process each elapsed tick
        if (ticks_elapsed > 0) {
            for (int i = 0; i < ticks_elapsed; i++) {
                process_game_tick(info);
            }
        }
    }
}