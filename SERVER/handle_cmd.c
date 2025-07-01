#define _POSIX_C_SOURCE 200809L
/*
** EPITECH PROJECT, 2024
** B-YEP-400-COT-4-1-zappy-evans.zime
** File description:
** handle_cmd.c
*/
#include "include/my.h"
#include <time.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

extern info_t *global_info;  // Declare global info pointer

const command_t commands[] = {
    {"Forward", forward_cmd, 7},
    {"Right", right_cmd, 7},
    {"Left", left_cmd, 7},
    {"Look", look_cmd, 7},
    {"Inventory", inventory_cmd, 1},
    {"Broadcast", broadcast_cmd, 7},
    {"Connect_nbr", connect_cmd, 1},
    {"Fork", execute_fork, 42},
    {"Eject", eject_cmd, 7},
    {"Take", handle_take, 7},
    {"Set", handle_set, 7},
    {"Incantation", incantation_cmd, 300}
};

static void clean_cmd_string(char *dest, const char *src)
{
    char *pos;
    strncpy(dest, src, 127);
    dest[127] = '\0';
    if ((pos = strchr(dest, '\n'))) *pos = '\0';
    if ((pos = strchr(dest, '\r'))) *pos = '\0';
}

void queue_command(player_t *player, char *command, int freq, int cmd_index)
{
    command_node_t *new_cmd = malloc(sizeof(command_node_t));
    if (!new_cmd) return;

    new_cmd->command = strdup(commands[cmd_index].command);
    new_cmd->buffer = strdup(command);
    new_cmd->exec_time = commands[cmd_index].exec_time;
    new_cmd->next = NULL;

    if (!player->command_queue) {
        player->command_queue = new_cmd;
    } else {
        command_node_t *curr = player->command_queue;
        while (curr->next) curr = curr->next;
        curr->next = new_cmd;
    }
}

void add_cmd(int player_id, char *command, int freq, info_t *info)
{
    char cmd_clean[128];
    clean_cmd_string(cmd_clean, command);
    
    for (size_t i = 0; i < sizeof(commands)/sizeof(commands[0]); i++) {
        if (strncmp(cmd_clean, commands[i].command, strlen(commands[i].command)) == 0) {
            queue_command(&info->game.players[player_id], command, freq, i);
            return;
        }
    }
    printf("Unknown command: %s\n", cmd_clean);
}

void execute_command(info_t *info, player_t *player, command_node_t *cmd)
{
    for (size_t j = 0; j < sizeof(commands)/sizeof(commands[0]); j++) {
        if (strcmp(cmd->command, commands[j].command) == 0) {
            commands[j].function(
                info->data_socket[player->id],
                player->id,
                cmd->buffer,
                info
            );
            break;
        }
    }
}

void process_commands(info_t *info)
{
    for (int i = 0; i < CLIENTS; i++) {
        if (!info->valid[i]) continue;
        
        player_t *player = &info->game.players[i];
        command_node_t *curr = player->command_queue;
        command_node_t *prev = NULL;
        
        while (curr) {
            // Convert exec_time to ticks (based on frequency)
            curr->exec_time--;
            
            if (curr->exec_time <= 0) {
                execute_command(info, player, curr);
                
                // Remove from queue
                command_node_t *to_free = curr;
                if (prev) {
                    prev->next = curr->next;
                    curr = curr->next;
                } else {
                    player->command_queue = curr->next;
                    curr = player->command_queue;
                }
                free(to_free->command);
                free(to_free->buffer);
                free(to_free);
            } else {
                prev = curr;
                curr = curr->next;
            }
        }
    }
}