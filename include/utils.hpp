/*
** EPITECH PROJECT, 2024
** my_pgp
** File description:
** utils
*/

#ifndef UTILS_HPP_
#define UTILS_HPP_

#include <string>
#include <vector>

class Utils {
public:
    static std::string read_stdin();
    static std::vector<unsigned char> hex_to_bytes(const std::string& hex);
    static std::string bytes_to_hex(const std::vector<unsigned char>& bytes);
    static bool is_valid_hex(const std::string& hex);
    static void validate_hex_string(const std::string& hex);
};

#endif /* !UTILS_HPP_ */
