/*
** EPITECH PROJECT, 2024
** my_pgp
** File description:
** xor_cipher
*/

#ifndef XOR_CIPHER_HPP_
#define XOR_CIPHER_HPP_

#include <string>
#include <vector>

class XorCipher {
public:
    XorCipher(const std::string& key);
    std::string cipher_block(const std::string& message);
    std::string decipher_block(const std::string& message);
    std::string cipher_stream(const std::string& message);
    std::string decipher_stream(const std::string& message);

private:
    std::vector<unsigned char> key_;
    
    std::vector<unsigned char> pad_data(
        const std::vector<unsigned char>& data,
        size_t target_size);
    std::vector<unsigned char> xor_operation(
        const std::vector<unsigned char>& data,
        const std::vector<unsigned char>& key);
};

#endif /* !XOR_CIPHER_HPP_ */
