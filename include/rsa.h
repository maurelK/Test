/*
** EPITECH PROJECT, 2024
** my_pgp
** File description:
** RSA header
*/

#ifndef RSA_H
    #define RSA_H

    #include "my_pgp.h"

int rsa_keygen(t_args *args);
int rsa_cipher(t_args *args);
int rsa_decipher(t_args *args);

void compute_carmichael(mpz_t lambda, mpz_t p, mpz_t q);
void find_fermat_e(mpz_t e, mpz_t lambda);
void compute_d(mpz_t d, mpz_t e, mpz_t lambda);

void gcd_extended(mpz_t gcd, mpz_t x, mpz_t y, mpz_t a, mpz_t b);
void lcm_compute(mpz_t result, mpz_t a, mpz_t b);

t_rsa_key *parse_rsa_key(char *key_str);
void free_rsa_key(t_rsa_key *key);

char *mpz_to_hex_le(mpz_t num);
void hex_le_to_mpz(mpz_t num, char *hex);

#endif
