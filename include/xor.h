/*
** EPITECH PROJECT, 2024
** my_pgp
** File description:
** XOR header
*/

#ifndef XOR_H
    #define XOR_H

    #include "my_pgp.h"

int xor_cipher(t_args *args);
int xor_decipher(t_args *args);
int xor_block_mode(t_args *args, int is_cipher);

void xor_bytes(unsigned char *msg, unsigned char *key, int len);
t_data *xor_stream(t_data *input, unsigned char *key, int key_len);

#endif
