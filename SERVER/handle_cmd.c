/*
** EPITECH PROJECT, 2024
** B-YEP-400-COT-4-1-zappy-evans.zime
** File description:
** handle_cmd.c
*/

#include "include/my.h"

pendingcmd_t pending_cmd[CLIENTS * 10];
int pending_c = 0;
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
    pos = strchr(dest, '\n');
    if (pos)
        *pos = '\0';
    pos = strchr(dest, '\r');
    if (pos)
        *pos = '\0';
}

void queue_command(int player_id, char *command, int freq, int cmd_index)
{
    if (pending_c >= CLIENTS * 10)
        return;
    pending_cmd[pending_c].player_id = player_id;
    pending_cmd[pending_c].command = strdup(commands[cmd_index].command);
    pending_cmd[pending_c].buffer = strdup(command);
    pending_cmd[pending_c].remaining_time =
    commands[cmd_index].exec_time / freq;
    pending_c++;
}

void add_cmd(int player_id, char *command, int freq)
{
    char cmd_clean[128];
    int cmd_count = sizeof(commands) / sizeof(commands[0]);
    int len;

    clean_cmd_string(cmd_clean, command);
    for (int i = 0; i < cmd_count; i++) {
        len = strlen(commands[i].command);
        if (strncmp(cmd_clean, commands[i].command, len) == 0) {
            queue_command(player_id, command, freq, i);
            return;
        }
    }
    printf("Commande inconnue: %s\n", cmd_clean);
}

static void execute_command(info_t *info, int index)
{
    for (size_t j = 0; j < sizeof(commands) / sizeof(commands[0]); j++) {
        if (strncmp(pending_cmd[index].command,
            commands[j].command, 4) == 0) {
            commands[j].function(
                info->data_socket[pending_cmd[index].player_id],
                pending_cmd[index].player_id,
                pending_cmd[index].buffer,
                info
            );
            break;
        }
    }
    free(pending_cmd[index].command);
    free(pending_cmd[index].buffer);
    for (int j = index; j < pending_c - 1; j++)
        pending_cmd[j] = pending_cmd[j + 1];
    pending_c--;
}

void process_commands(info_t *info)
{
    for (int i = 0; i < pending_c; i++) {
        pending_cmd[i].remaining_time--;
        if (pending_cmd[i].remaining_time <= 0) {
            execute_command(info, i);
            i--;
        }
    }
}
