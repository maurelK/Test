/*
** EPITECH PROJECT, 2024
** my_pgp
** File description:
** Main header file
*/

#ifndef MY_PGP_H
    #define MY_PGP_H

    #include <stdio.h>
    #include <stdlib.h>
    #include <string.h>
    #include <unistd.h>
    #include <gmp.h>

    #define EXIT_ERROR 84
    #define EXIT_SUCCESS 0

typedef struct s_args {
    char *crypto_system;
    char *mode;
    int block_mode;
    char *key;
    char *p_prime;
    char *q_prime;
} t_args;

typedef struct s_data {
    unsigned char *bytes;
    int len;
} t_data;

typedef struct s_rsa_key {
    mpz_t exponent;
    mpz_t modulus;
} t_rsa_key;

int parse_args(int ac, char **av, t_args *args);
int dispatch_crypto(t_args *args);

#endif
