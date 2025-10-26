/*
** EPITECH PROJECT, 2024
** my_pgp
** File description:
** xor_cipher
*/

#include "xor_cipher.hpp"
#include "utils.hpp"
#include "error_handler.hpp"
#include <algorithm>

XorCipher::XorCipher(const std::string& key)
{ 
    key_ = Utils::hex_to_bytes(key);
}

std::vector<unsigned char> XorCipher::pad_data(
    const std::vector<unsigned char>& data,
    size_t target_size)
{
    std::vector<unsigned char> padded = data;
    while (padded.size() < target_size)
        padded.push_back(0);
    return padded;
}

std::vector<unsigned char> XorCipher::xor_operation(
    const std::vector<unsigned char>& data,
    const std::vector<unsigned char>& key)
{
    std::vector<unsigned char> result;
    for (size_t i = 0; i < data.size(); i++)
        result.push_back(data[i] ^ key[i % key.size()]);
    return result;
}

std::string XorCipher::cipher_block(const std::string& message)
{
    std::vector<unsigned char> msg_bytes(message.begin(), message.end());

    // here- was the issues benaya REVERSE the message bytes to convert to little-endian
    std::reverse(msg_bytes.begin(), msg_bytes.end());

    if (msg_bytes.size() != key_.size()) {
        msg_bytes = pad_data(msg_bytes, key_.size());
    }
    auto result = xor_operation(msg_bytes, key_);
    return Utils::bytes_to_hex(result);
}


std::string XorCipher::decipher_block(const std::string& message)
{
    auto cipher_bytes = Utils::hex_to_bytes(message);

    if (cipher_bytes.size() != key_.size()) {
        cipher_bytes = pad_data(cipher_bytes, key_.size());
    }
    auto result = xor_operation(cipher_bytes, key_);

    // here ben - you have to turn it back again otherwise your msg will be wrong like has we have it actually REVERSE the result back to big-endian for text output
    std::reverse(result.begin(), result.end());
    while (!result.empty() && result.back() == 0) {
        result.pop_back();
    }
    return std::string(result.begin(), result.end());
}


std::string XorCipher::cipher_stream(const std::string& message)
{
    std::vector<unsigned char> msg_bytes(message.begin(), message.end());
    std::vector<unsigned char> result;

    for (size_t i = 0; i < msg_bytes.size(); i += key_.size()) {
	size_t block_size = std::min(key_.size(), msg_bytes.size() - i);
        std::vector<unsigned char> block(
            msg_bytes.begin() + i,
            msg_bytes.begin() + i + block_size);

        // Reverse each block to little-endian
        std::reverse(block.begin(), block.end());
        if (block.size() < key_.size())
            block = pad_data(block, key_.size());

        auto xored = xor_operation(block, key_);
        result.insert(result.end(), xored.begin(), xored.end());
    }
    return Utils::bytes_to_hex(result);
}

std::string XorCipher::decipher_stream(const std::string& message)
{
    auto cipher_bytes = Utils::hex_to_bytes(message);
    std::vector<unsigned char> result;

    for (size_t i = 0; i < cipher_bytes.size(); i += key_.size()) {
        size_t block_size = std::min(key_.size(), cipher_bytes.size() - i);
        std::vector<unsigned char> block(
            cipher_bytes.begin() + i,
            cipher_bytes.begin() + i + block_size);
        auto xored = xor_operation(block, key_);

        // Reverse each decrypted block back to big-endian
        std::reverse(xored.begin(), xored.end());
        if (i + key_.size() >= cipher_bytes.size()) {
            while (!xored.empty() && xored.back() == 0)
                xored.pop_back();
        }
        result.insert(result.end(), xored.begin(), xored.end());
    }
    return std::string(result.begin(), result.end());
}
