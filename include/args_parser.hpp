/*
** EPITECH PROJECT, 2024
** my_pgp
** File description:
** args_parser
*/

#ifndef ARGS_PARSER_HPP_
#define ARGS_PARSER_HPP_

#include <string>
#include <vector>

struct Arguments {
    std::string crypto_system;
    std::string mode;
    bool block_mode;
    std::string key;
    std::string prime_p;
    std::string prime_q;
};

class ArgsParser {
public:
    ArgsParser(int argc, char **argv);
    Arguments parse();
    void print_usage() const;

private:
    int argc_;
    char **argv_;
    Arguments args_;


    void parse_crypto_system();
    void parse_mode();
    void check_block_flag();
    void parse_key_or_primes();
    bool is_help_requested() const;
};

#endif /* !ARGS_PARSER_HPP_ */
