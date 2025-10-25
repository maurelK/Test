/*
** EPITECH PROJECT, 2024
** my_pgp
** File description:
** error_handler
*/

#ifndef ERROR_HANDLER_HPP_
#define ERROR_HANDLER_HPP_

#include <string>

class Error {
public:
    static const int SUCCESS = 0;
    static const int FAILURE = 84;

    static void print_error(const std::string& message);
    static void exit_with_error(const std::string& message);
    static void check_args_count(int argc, int min, int max);
    static void check_mode_valid(const std::string& mode);
    static void check_crypto_valid(const std::string& crypto);
};

#endif /* !ERROR_HANDLER_HPP_ */
