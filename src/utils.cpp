/*
** EPITECH PROJECT, 2024
** my_pgp
** File description:
** utils
*/

#include "utils.hpp"
#include "error_handler.hpp"
#include <iostream>
#include <sstream>
#include <iomanip>
#include <cctype>
#include <cstdint>

std::string Utils::read_stdin()
{
    std::string line;
    std::getline(std::cin, line);
    return line;
}

bool Utils::is_valid_hex(const std::string& hex)
{
    for (char c : hex) {
        if (!std::isxdigit(c))
            return false;
    }
    return hex.length() % 2 == 0;
}

void Utils::validate_hex_string(const std::string& hex)
{
    if (!is_valid_hex(hex))
        Error::exit_with_error("Invalid hexadecimal string");
}

std::vector<unsigned char> Utils::hex_to_bytes(const std::string& hex)
{
    validate_hex_string(hex);
    std::vector<unsigned char> bytes;
    
    for (size_t i = 0; i < hex.length(); i += 2) {
        std::string byte_str = hex.substr(i, 2);
        unsigned char byte = std::stoi(byte_str, nullptr, 16);
        bytes.push_back(byte);
    }
    return bytes;
}

std::string Utils::bytes_to_hex(const std::vector<unsigned char>& bytes)
{
    std::stringstream ss;
    
    for (unsigned char byte : bytes) {
        ss << std::hex << std::setw(2) << std::setfill('0');
        ss << static_cast<int>(byte);
    }
    return ss.str();
}

u_int8_t multiplication_by_galois(u_int8_t bytes)
{
    uint8_t overflow = bytes & 0x80;
    
    bytes <<= 1;
    if ( overflow & 0x80) {
        bytes ^= 0x1B;
    }
    return bytes;
}
