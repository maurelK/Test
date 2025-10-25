/*
** EPITECH PROJECT, 2024
** my_pgp
** File description:
** args_parser
*/

#include "args_parser.hpp"
#include "error_handler.hpp"
#include <iostream>

ArgsParser::ArgsParser(int argc, char **argv)
    : argc_(argc), argv_(argv)
{
    args_.block_mode = false;
}

bool ArgsParser::is_help_requested() const
{
    return argc_ == 2 && std::string(argv_[1]) == "-h";
}

void ArgsParser::print_usage() const
{
    std::cout << "USAGE\n";
    std::cout << "    ./my_pgp CRYPTO_SYSTEM MODE [OPTIONS] [key]\n\n";
    std::cout << "DESCRIPTION\n";
    std::cout << "    Cipher or decipher MESSAGE using a given CRYPTO_SYSTEM.\n";
    std::cout << "    The MESSAGE is read from the standard input.\n\n";
    std::cout << "CRYPTO_SYSTEM\n";
    std::cout << "    \"xor\"        computation using XOR algorithm\n";
    std::cout << "    \"aes\"        computation using 128-bit AES algorithm\n";
    std::cout << "    \"rsa\"        computation using RSA algorithm\n";
    std::cout << "    \"pgp-xor\"    computation using both RSA and XOR algorithm\n";
    std::cout << "    \"pgp-aes\"    computation using both RSA and 128-bit AES algorithm\n\n";
    std::cout << "MODE\n";
    std::cout << "    -c           MESSAGE is clear and we want to cipher it\n";
    std::cout << "    -d           MESSAGE is ciphered and we want to decipher it\n";
    std::cout << "    -g P Q       for RSA only: generate a public and private key pair\n\n";
    std::cout << "OPTIONS\n";
    std::cout << "    -b           for XOR, AES and PGP, only works on one block\n\n";
    std::cout << "key              Key used to cipher/decipher MESSAGE\n";
}

void ArgsParser::parse_crypto_system()
{
    if (argc_ < 2)
        Error::exit_with_error("Missing crypto system");
    args_.crypto_system = argv_[1];
    Error::check_crypto_valid(args_.crypto_system);
}

void ArgsParser::parse_mode()
{
    if (argc_ < 3)
        Error::exit_with_error("Missing mode");
    args_.mode = argv_[2];
    Error::check_mode_valid(args_.mode);
}

void ArgsParser::check_block_flag()
{
    for (int i = 3; i < argc_; i++) {
        if (std::string(argv_[i]) == "-b") {
            args_.block_mode = true;
            return;
        }
    }
}

void ArgsParser::parse_key_or_primes()
{
    if (args_.mode == "-g") {
        if (argc_ < 5)
            Error::exit_with_error("Missing prime numbers for -g mode");
        args_.prime_p = argv_[3];
        args_.prime_q = argv_[4];
    } else {
        int key_index = args_.block_mode ? 4 : 3;
        if (argc_ <= key_index)
            Error::exit_with_error("Missing key");
        args_.key = argv_[key_index];
    }
}

Arguments ArgsParser::parse()
{
    if (is_help_requested()) {
        print_usage();
        exit(Error::SUCCESS);
    }
    Error::check_args_count(argc_, 3, 6);
    parse_crypto_system();
    parse_mode();
    check_block_flag();
    parse_key_or_primes();
    return args_;
}
