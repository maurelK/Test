/*
** EPITECH PROJECT, 2024
** my_pgp
** File description:
** error_handler
*/

#include "error_handler.hpp"
#include <iostream>
#include <cstdlib>

void Error::print_error(const std::string& message)
{
    std::cerr << "Error: " << message << std::endl;
}

void Error::exit_with_error(const std::string& message)
{
    print_error(message);
    exit(FAILURE);
}

void Error::check_args_count(int argc, int min, int max)
{
    if (argc < min || argc > max)
        exit_with_error("Invalid number of arguments");
}

void Error::check_mode_valid(const std::string& mode)
{
    if (mode != "-c" && mode != "-d" && mode != "-g")
        exit_with_error("Invalid mode: use -c, -d, or -g");
}

void Error::check_crypto_valid(const std::string& crypto)
{
    if (crypto != "xor" && crypto != "aes" && crypto != "rsa" &&
        crypto != "pgp-xor" && crypto != "pgp-aes")
        exit_with_error("Invalid crypto system");
}
