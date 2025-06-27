/*
** EPITECH PROJECT, 2024
** B-YEP-400-COT-4-1-zappy-evans.zime [WSL: Ubuntu]
** File description:
** handle_cmd.c
*/

#include "include/my.h"

pendingcmd_t pending_cmd[CLIENTS * 10];
int pending_c = 0;

command_t commands[] = {
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

void add_cmd(int player_id, char *command, int freq)
{
    int commands_count = sizeof(commands) / sizeof(commands[0]);
    char cmd_clean[128];
    strncpy(cmd_clean, command, sizeof(cmd_clean) - 1);
    cmd_clean[sizeof(cmd_clean) - 1] = '\0';

    char *pos;
    if ((pos = strchr(cmd_clean, '\n')) != NULL)
        *pos = '\0';
    if ((pos = strchr(cmd_clean, '\r')) != NULL)
        *pos = '\0';
    for (int i = 0; i < commands_count; i++) {
        int len = strlen(commands[i].command);
        if (strncmp(cmd_clean, commands[i].command, len) == 0) {
            if (pending_c >= CLIENTS * 10)
                return;

            pending_cmd[pending_c].player_id = player_id;
            pending_cmd[pending_c].command = strdup(commands[i].command);
            pending_cmd[pending_c].buffer = strdup(command);
            pending_cmd[pending_c].remaining_time = commands[i].exec_time / freq;
            pending_c++;
            return;
        }
    }
    printf("Commande inconnue: %s\n", cmd_clean);
}



void process_commands(info_t* info)
{
    for (int i = 0; i < pending_c; i++) {
        pending_cmd[i].remaining_time--;
        if (pending_cmd[i].remaining_time <= 0) {
            for (size_t j = 0; j < sizeof(commands) / sizeof(commands[0]); j++) {
                if (strncmp(pending_cmd[i].command, commands[j].command, 4) == 0) {
                    commands[j].function(
                        info->data_socket[pending_cmd[i].player_id],
                        pending_cmd[i].player_id,
                        pending_cmd[i].buffer,
                        info
                    );
                    break;
                }
            }
            free(pending_cmd[i].command);
            free(pending_cmd[i].buffer);
            for (int j = i; j < pending_c - 1; j++) {
                pending_cmd[j] = pending_cmd[j + 1];
            }
            pending_c--;
            i--;
        }
    }
}

