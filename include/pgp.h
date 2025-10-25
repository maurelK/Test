/*
** EPITECH PROJECT, 2024
** my_pgp
** File description:
** PGP header
*/

#ifndef PGP_H
    #define PGP_H

    #include "my_pgp.h"

int pgp_xor_cipher(t_args *args);
int pgp_xor_decipher(t_args *args);

typedef struct s_pgp_key {
    char *symmetric_key;
    char *rsa_key;
} t_pgp_key;

t_pgp_key *parse_pgp_key(char *key_str);
void free_pgp_key(t_pgp_key *key);

#endif
