/*
** EPITECH PROJECT, 2024
** my_pgp
** File description:
** pgp_cipher
*/

#ifndef PGP_CIPHER_HPP_
#define PGP_CIPHER_HPP_

#include <string>

class PgpCipher {
public:
    PgpCipher();
    
    std::string cipher_xor(
        const std::string& message,
        const std::string& combined_key,
        bool block_mode);
    std::string decipher_xor(
        const std::string& cipher_text,
        const std::string& combined_key,
        bool block_mode);
    
    std::string cipher_aes(
        const std::string& message,
        const std::string& combined_key,
        bool block_mode);
    std::string decipher_aes(
        const std::string& cipher_text,
        const std::string& combined_key,
        bool block_mode);

private:
    void parse_combined_key(
        const std::string& combined_key,
        std::string& sym_key,
        std::string& rsa_key);
};

#endif /* !PGP_CIPHER_HPP_ */
