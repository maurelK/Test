/*
** EPITECH PROJECT, 2024
** my_pgp
** File description:
** Utils header
*/

#ifndef UTILS_H
    #define UTILS_H

    #include "my_pgp.h"

void print_error(char *msg);
void exit_error(char *msg);

t_data *read_stdin(void);
void free_data(t_data *data);

unsigned char *hex_to_bytes(char *hex, int *len);
char *bytes_to_hex(unsigned char *bytes, int len);

void reverse_bytes(unsigned char *bytes, int len);
int hex_char_to_int(char c);
char int_to_hex_char(int n);

#endif
