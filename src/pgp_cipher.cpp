/*
** EPITECH PROJECT, 2024
** my_pgp
** File description:
** pgp_cipher
*/

#include "pgp_cipher.hpp"
#include "xor_cipher.hpp"
#include "rsa_cipher.hpp"
#include "error_handler.hpp"

PgpCipher::PgpCipher() {}

void PgpCipher::parse_combined_key(
    const std::string& combined_key,
    std::string& sym_key,
    std::string& rsa_key)
{
    size_t pos = combined_key.find(':');
    if (pos == std::string::npos)
        Error::exit_with_error("Invalid PGP key format (missing ':')");
    
    sym_key = combined_key.substr(0, pos);
    rsa_key = combined_key.substr(pos + 1);
}

std::string PgpCipher::cipher_xor(
    const std::string& message,
    const std::string& combined_key,
    bool block_mode)
{
    std::string sym_key, rsa_public_key;
    parse_combined_key(combined_key, sym_key, rsa_public_key);
    
    RsaCipher rsa;
    std::string encrypted_key = rsa.cipher(sym_key, rsa_public_key);
    
    XorCipher xor_cipher(sym_key);
    std::string encrypted_msg = block_mode ?
        xor_cipher.cipher_block(message) :
        xor_cipher.cipher_stream(message);
    
    return encrypted_key + ":" + encrypted_msg;
}

std::string PgpCipher::decipher_xor(
    const std::string& cipher_text,
    const std::string& combined_key,
    bool block_mode)
{
    size_t pos = cipher_text.find(':');
    if (pos == std::string::npos)
        Error::exit_with_error("Invalid PGP cipher format");
    
    std::string encrypted_key = cipher_text.substr(0, pos);
    std::string encrypted_msg = cipher_text.substr(pos + 1);
    
    std::string sym_key_hex, rsa_private_key;
    parse_combined_key(combined_key, sym_key_hex, rsa_private_key);
    
    RsaCipher rsa;
    std::string decrypted_key = rsa.decipher(encrypted_key, rsa_private_key);
    
    XorCipher xor_cipher(decrypted_key);
    return block_mode ?
        xor_cipher.decipher_block(encrypted_msg) :
        xor_cipher.decipher_stream(encrypted_msg);
}

std::string PgpCipher::cipher_aes(
    const std::string& message,
    const std::string& combined_key,
    bool block_mode)
{
    (void)message;
    (void)combined_key;
    (void)block_mode;
    Error::exit_with_error("AES not yet implemented");
    return "";
}

std::string PgpCipher::decipher_aes(
    const std::string& cipher_text,
    const std::string& combined_key,
    bool block_mode)
{
    (void)cipher_text;
    (void)combined_key;
    (void)block_mode;
    Error::exit_with_error("AES not yet implemented");
    return "";
}
