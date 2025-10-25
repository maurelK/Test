/*
** EPITECH PROJECT, 2024
** my_pgp
** File description:
** main
*/

#include "args_parser.hpp"
#include "error_handler.hpp"
#include "xor_cipher.hpp"
#include "rsa_cipher.hpp"
#include "pgp_cipher.hpp"
#include "utils.hpp"
#include <iostream>

void handle_xor_cipher(const Arguments& args)
{
    XorCipher xor_cipher(args.key);
    std::string message = Utils::read_stdin();
    std::string result;
    
    if (args.mode == "-c") {
        result = args.block_mode ? 
            xor_cipher.cipher_block(message) :
            xor_cipher.cipher_stream(message);
    } else {
        result = args.block_mode ?
            xor_cipher.decipher_block(message) :
            xor_cipher.decipher_stream(message);
    }
    std::cout << result << std::endl;
}

void handle_rsa_generate(const Arguments& args)
{
    RsaCipher rsa;
    RsaKeys keys = rsa.generate_keys(args.prime_p, args.prime_q);
    
    std::cout << "public key: " << keys.public_key << std::endl;
    std::cout << "private key: " << keys.private_key << std::endl;
}

void handle_rsa_cipher(const Arguments& args)
{
    RsaCipher rsa;
    std::string message = Utils::read_stdin();
    std::string result;
    
    if (args.mode == "-c")
        result = rsa.cipher(message, args.key);
    else
        result = rsa.decipher(message, args.key);
    
    std::cout << result << std::endl;
}

void handle_pgp_xor(const Arguments& args)
{
    PgpCipher pgp;
    std::string message = Utils::read_stdin();
    std::string result;
    
    if (args.mode == "-c") {
        result = pgp.cipher_xor(message, args.key, args.block_mode);
    } else {
        result = pgp.decipher_xor(message, args.key, args.block_mode);
    }
    std::cout << result << std::endl;
}

void handle_pgp_aes(const Arguments& args)
{
    PgpCipher pgp;
    std::string message = Utils::read_stdin();
    std::string result;
    
    if (args.mode == "-c") {
        result = pgp.cipher_aes(message, args.key, args.block_mode);
    } else {
        result = pgp.decipher_aes(message, args.key, args.block_mode);
    }
    std::cout << result << std::endl;
}

void process_crypto_system(const Arguments& args)
{
    if (args.crypto_system == "xor") {
        handle_xor_cipher(args);
    } else if (args.crypto_system == "rsa") {
        if (args.mode == "-g")
            handle_rsa_generate(args);
        else
            handle_rsa_cipher(args);
    } else if (args.crypto_system == "pgp-xor") {
        handle_pgp_xor(args);
    } else if (args.crypto_system == "pgp-aes") {
        handle_pgp_aes(args);
    } else {
        Error::exit_with_error("Crypto system not yet implemented");
    }
}

int main(int argc, char **argv)
{
    try {
        ArgsParser parser(argc, argv);
        Arguments args = parser.parse();
        process_crypto_system(args);
        return Error::SUCCESS;
    } catch (const std::exception& e) {
        Error::print_error(e.what());
        return Error::FAILURE;
    }
}
