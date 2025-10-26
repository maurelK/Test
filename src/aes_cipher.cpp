#include "aes_cipher.hpp"
#include "utils.hpp"
#include "error_handler.hpp"
#include <algorithm>
#include <stdexcept>
#include <cstring>
#include <sstream>
#include <iomanip>

AesCipher::AesCipher(const std::string& key)
{
    // The key is provided as four 32-bit numbers in little-endian hex
    key_ = convert_32bit_numbers_to_bytes(key);
    
    if (key_.size() != 16) {
        Error::exit_with_error("AES key must be 128-bit (16 bytes)");
    }
}

// Convert four 32-bit numbers in little-endian hex to 16 bytes
std::vector<unsigned char> AesCipher::convert_32bit_numbers_to_bytes(const std::string& numbers_hex)
{
    // Remove any hyphens if present
    std::string clean_hex = numbers_hex;
    clean_hex.erase(std::remove(clean_hex.begin(), clean_hex.end(), '-'), clean_hex.end());
    
    if (clean_hex.length() != 32) {
        Error::exit_with_error("AES key must be 128-bit represented as four 32-bit numbers");
    }
    
    std::vector<unsigned char> result;
    for (size_t i = 0; i < 32; i += 8) {
        std::string number_hex = clean_hex.substr(i, 8);
        // Convert little-endian 32-bit number to bytes
        for (size_t j = 0; j < 8; j += 2) {
            std::string byte_hex = number_hex.substr(j, 2);
            unsigned char byte = std::stoi(byte_hex, nullptr, 16);
            result.push_back(byte);
        }
    }
    
    return result;
}

// Convert 16 bytes to four 32-bit numbers in little-endian hex
std::string AesCipher::convert_bytes_to_32bit_numbers(const std::vector<unsigned char>& bytes)
{
    if (bytes.size() != 16) {
        Error::exit_with_error("Need exactly 16 bytes for four 32-bit numbers");
    }
    
    std::stringstream result;
    
    // Convert each group of 4 bytes to a 32-bit number in little-endian
    for (int i = 0; i < 4; i++) {
        uint32_t number = 0;
        for (int j = 0; j < 4; j++) {
            number |= (bytes[i * 4 + j] << (8 * j));
        }
        
        // Convert to hex and ensure 8 characters
        std::stringstream ss;
        ss << std::hex << std::setw(8) << std::setfill('0') << number;
        std::string number_hex = ss.str();
        
        result << number_hex;
    }
    
    return result.str();
}

// Convert message string to four 32-bit numbers in little-endian
std::vector<unsigned char> AesCipher::message_to_32bit_numbers(const std::string& message)
{
    std::vector<unsigned char> msg_bytes(message.begin(), message.end());
    
    // Pad to exactly 16 bytes with zeros
    while (msg_bytes.size() < 16) {
        msg_bytes.push_back(0);
    }
    
    // The message bytes are already in the correct order for 32-bit numbers
    // Each group of 4 bytes becomes one 32-bit number in little-endian
    return msg_bytes;
}

// Convert four 32-bit numbers back to message string
std::string AesCipher::numbers_to_message(const std::vector<unsigned char>& bytes)
{
    // Remove trailing zeros (padding)
    std::vector<unsigned char> result = bytes;
    while (!result.empty() && result.back() == 0) {
        result.pop_back();
    }
    
    return std::string(result.begin(), result.end());
}


// Simple padding with zeros for block mode
std::vector<unsigned char> AesCipher::pad_data(const std::vector<unsigned char>& data, size_t block_size)
{
    std::vector<unsigned char> padded = data;
    while (padded.size() < block_size) {
        padded.push_back(0);
    }
    return padded;
}

std::vector<unsigned char> AesCipher::remove_padding(const std::vector<unsigned char>& data)
{
    if (data.empty()) return data;
    
    // Remove trailing zeros
    std::vector<unsigned char> result = data;
    while (!result.empty() && result.back() == 0) {
        result.pop_back();
    }
    return result;
}

void AesCipher::state_to_bytes(const uint8_t state[4][4], std::vector<unsigned char>& bytes)
{
    bytes.clear();
    for (int col = 0; col < 4; col++) {
        for (int row = 0; row < 4; row++) {
            bytes.push_back(state[row][col]);
        }
    }
}

void AesCipher::bytes_to_state(const std::vector<unsigned char>& bytes, uint8_t state[4][4])
{
    for (int col = 0; col < 4; col++) {
        for (int row = 0; row < 4; row++) {
            state[row][col] = bytes[col * 4 + row];
        }
    }
}

// AES Core Functions - Fixed implementations
void AesCipher::SubBytes(uint8_t state[4][4])
{
    for (int i = 0; i < 4; i++) {
        for (int j = 0; j < 4; j++) {
            state[i][j] = sBox[state[i][j]];
        }
    }
}

void AesCipher::InvSubBytes(uint8_t state[4][4])
{
    for (int i = 0; i < 4; i++) {
        for (int j = 0; j < 4; j++) {
            state[i][j] = invSBox[state[i][j]];
        }
    }
}

void AesCipher::ShiftRows(uint8_t state[4][4])
{
    uint8_t temp;
    
    // Shift row 1 by 1
    temp = state[1][0];
    state[1][0] = state[1][1];
    state[1][1] = state[1][2];
    state[1][2] = state[1][3];
    state[1][3] = temp;
    
    // Shift row 2 by 2
    temp = state[2][0];
    state[2][0] = state[2][2];
    state[2][2] = temp;
    temp = state[2][1];
    state[2][1] = state[2][3];
    state[2][3] = temp;
    
    // Shift row 3 by 3
    temp = state[3][0];
    state[3][0] = state[3][3];
    state[3][3] = state[3][2];
    state[3][2] = state[3][1];
    state[3][1] = temp;
}

void AesCipher::InvShiftRows(uint8_t state[4][4])
{
    uint8_t temp;
    
    // Inverse shift row 1
    temp = state[1][3];
    state[1][3] = state[1][2];
    state[1][2] = state[1][1];
    state[1][1] = state[1][0];
    state[1][0] = temp;
    
    // Inverse shift row 2
    temp = state[2][0];
    state[2][0] = state[2][2];
    state[2][2] = temp;
    temp = state[2][1];
    state[2][1] = state[2][3];
    state[2][3] = temp;
    
    // Inverse shift row 3
    temp = state[3][0];
    state[3][0] = state[3][1];
    state[3][1] = state[3][2];
    state[3][2] = state[3][3];
    state[3][3] = temp;
}

// Fixed GF multiplication
uint8_t AesCipher::gfMultiply(uint8_t a, uint8_t b)
{
    uint8_t p = 0;
    uint8_t counter;
    uint8_t hi_bit_set;
    
    for (counter = 0; counter < 8; counter++) {
        if (b & 1) {
            p ^= a;
        }
        hi_bit_set = (a & 0x80);
        a <<= 1;
        if (hi_bit_set) {
            a ^= 0x1b; /* x^8 + x^4 + x^3 + x + 1 */
        }
        b >>= 1;
    }
    return p;
}

void AesCipher::MixColumns(uint8_t state[4][4])
{
    uint8_t a[4];
    uint8_t b[4];
    
    for (int c = 0; c < 4; c++) {
        a[0] = state[0][c];
        a[1] = state[1][c];
        a[2] = state[2][c];
        a[3] = state[3][c];
        
        b[0] = gfMultiply(0x02, a[0]) ^ gfMultiply(0x03, a[1]) ^ a[2] ^ a[3];
        b[1] = a[0] ^ gfMultiply(0x02, a[1]) ^ gfMultiply(0x03, a[2]) ^ a[3];
        b[2] = a[0] ^ a[1] ^ gfMultiply(0x02, a[2]) ^ gfMultiply(0x03, a[3]);
        b[3] = gfMultiply(0x03, a[0]) ^ a[1] ^ a[2] ^ gfMultiply(0x02, a[3]);
        
        state[0][c] = b[0];
        state[1][c] = b[1];
        state[2][c] = b[2];
        state[3][c] = b[3];
    }
}

void AesCipher::InvMixColumns(uint8_t state[4][4])
{
    uint8_t a[4];
    uint8_t b[4];
    
    for (int c = 0; c < 4; c++) {
        a[0] = state[0][c];
        a[1] = state[1][c];
        a[2] = state[2][c];
        a[3] = state[3][c];
        
        b[0] = gfMultiply(0x0e, a[0]) ^ gfMultiply(0x0b, a[1]) ^ 
                gfMultiply(0x0d, a[2]) ^ gfMultiply(0x09, a[3]);
        b[1] = gfMultiply(0x09, a[0]) ^ gfMultiply(0x0e, a[1]) ^ 
                gfMultiply(0x0b, a[2]) ^ gfMultiply(0x0d, a[3]);
        b[2] = gfMultiply(0x0d, a[0]) ^ gfMultiply(0x09, a[1]) ^ 
                gfMultiply(0x0e, a[2]) ^ gfMultiply(0x0b, a[3]);
        b[3] = gfMultiply(0x0b, a[0]) ^ gfMultiply(0x0d, a[1]) ^ 
                gfMultiply(0x09, a[2]) ^ gfMultiply(0x0e, a[3]);
        
        state[0][c] = b[0];
        state[1][c] = b[1];
        state[2][c] = b[2];
        state[3][c] = b[3];
    }
}

void AesCipher::AddRoundKey(uint8_t state[4][4], const uint32_t* roundKey)
{
    for (int c = 0; c < 4; c++) {
        uint32_t key_word = roundKey[c];
        state[0][c] ^= (key_word >> 24) & 0xFF;
        state[1][c] ^= (key_word >> 16) & 0xFF;
        state[2][c] ^= (key_word >> 8) & 0xFF;
        state[3][c] ^= key_word & 0xFF;
    }
}

uint32_t AesCipher::SubWord(uint32_t word)
{
    return (sBox[(word >> 24) & 0xFF] << 24) |
           (sBox[(word >> 16) & 0xFF] << 16) |
           (sBox[(word >> 8) & 0xFF] << 8) |
           (sBox[word & 0xFF]);
}

uint32_t AesCipher::RotWord(uint32_t word)
{
    return ((word << 8) | (word >> 24));
}

uint32_t AesCipher::Rcon(int round)
{
    static const uint8_t rcon[10] = {
        0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0x80, 0x1b, 0x36
    };
    return rcon[round-1] << 24;
}

void AesCipher::KeyExpansion(const uint8_t key[16], uint32_t roundKey[44])
{
    int i = 0;
    
    // First 4 words are the key itself
    while (i < 4) {
        roundKey[i] = (key[4*i] << 24) | (key[4*i+1] << 16) | (key[4*i+2] << 8) | key[4*i+3];
        i++;
    }
    
    // Expand the rest
    while (i < 44) {
        uint32_t temp = roundKey[i-1];
        
        if (i % 4 == 0) {
            temp = SubWord(RotWord(temp)) ^ Rcon(i/4);
        }
        
        roundKey[i] = roundKey[i-4] ^ temp;
        i++;
    }
}
std::string AesCipher::cipher_block(const std::string& message)
{
    // Convert message to four 32-bit numbers (16 bytes)
    std::vector<unsigned char> msg_bytes = message_to_32bit_numbers(message);
    
    // Expand key
    uint32_t roundKey[44];
    KeyExpansion(key_.data(), roundKey);
    
    // Initialize state
    uint8_t state[4][4];
    bytes_to_state(msg_bytes, state);
    
    // AES encryption
    AddRoundKey(state, roundKey);
    
    for (int round = 1; round < Nr; round++) {
        SubBytes(state);
        ShiftRows(state);
        MixColumns(state);
        AddRoundKey(state, roundKey + round * 4);
    }
    
    SubBytes(state);
    ShiftRows(state);
    AddRoundKey(state, roundKey + Nr * 4);
    
    // Convert state to bytes
    std::vector<unsigned char> result_bytes;
    state_to_bytes(state, result_bytes);
    
    // Convert result to four 32-bit numbers in hex
    return convert_bytes_to_32bit_numbers(result_bytes);
}

std::string AesCipher::decipher_block(const std::string& message)
{
    // The input is four 32-bit numbers in hex, convert to bytes
    std::vector<unsigned char> cipher_bytes = convert_32bit_numbers_to_bytes(message);
    
    // Expand key
    uint32_t roundKey[44];
    KeyExpansion(key_.data(), roundKey);
    
    // Initialize state
    uint8_t state[4][4];
    bytes_to_state(cipher_bytes, state);
    
    // AES decryption
    AddRoundKey(state, roundKey + Nr * 4);
    
    for (int round = Nr-1; round > 0; round--) {
        InvShiftRows(state);
        InvSubBytes(state);
        AddRoundKey(state, roundKey + round * 4);
        InvMixColumns(state);
    }
    
    InvShiftRows(state);
    InvSubBytes(state);
    AddRoundKey(state, roundKey);
    
    // Convert state to bytes
    std::vector<unsigned char> result_bytes;
    state_to_bytes(state, result_bytes);
    
    // Convert back to message string
    return numbers_to_message(result_bytes);
}

std::string AesCipher::cipher_stream(const std::string& message)
{
    // For stream mode, process in 16-byte blocks
    std::vector<unsigned char> msg_bytes(message.begin(), message.end());
    std::vector<unsigned char> result;
    
    // Pad the entire message
    msg_bytes = pad_data(msg_bytes, ((msg_bytes.size() + 15) / 16) * 16);
    
    // Expand key once
    uint32_t roundKey[44];
    KeyExpansion(key_.data(), roundKey);
    
    // Process each block
    for (size_t i = 0; i < msg_bytes.size(); i += 16) {
        std::vector<unsigned char> block(msg_bytes.begin() + i, msg_bytes.begin() + i + 16);
        
        uint8_t state[4][4];
        bytes_to_state(block, state);
        
        // AES encryption
        AddRoundKey(state, roundKey);
        for (int round = 1; round < Nr; round++) {
            SubBytes(state);
            ShiftRows(state);
            MixColumns(state);
            AddRoundKey(state, roundKey + round * 4);
        }
        SubBytes(state);
        ShiftRows(state);
        AddRoundKey(state, roundKey + Nr * 4);
        
        std::vector<unsigned char> encrypted_block;
        state_to_bytes(state, encrypted_block);
        result.insert(result.end(), encrypted_block.begin(), encrypted_block.end());
    }
    
    // Convert result to 32-bit numbers
    return convert_bytes_to_32bit_numbers(result);
}

std::string AesCipher::decipher_stream(const std::string& message)
{
    // Convert input to bytes
    std::vector<unsigned char> cipher_bytes = convert_32bit_numbers_to_bytes(message);
    
    std::vector<unsigned char> result;
    
    // Expand key once
    uint32_t roundKey[44];
    KeyExpansion(key_.data(), roundKey);
    
    // Process each block
    for (size_t i = 0; i < cipher_bytes.size(); i += 16) {
        std::vector<unsigned char> block(cipher_bytes.begin() + i, cipher_bytes.begin() + i + 16);
        
        uint8_t state[4][4];
        bytes_to_state(block, state);
        
        // AES decryption
        AddRoundKey(state, roundKey + Nr * 4);
        for (int round = Nr-1; round > 0; round--) {
            InvShiftRows(state);
            InvSubBytes(state);
            AddRoundKey(state, roundKey + round * 4);
            InvMixColumns(state);
        }
        InvShiftRows(state);
        InvSubBytes(state);
        AddRoundKey(state, roundKey);
        
        std::vector<unsigned char> decrypted_block;
        state_to_bytes(state, decrypted_block);
        
        // For last block, remove padding
        if (i + 16 >= cipher_bytes.size()) {
            while (!decrypted_block.empty() && decrypted_block.back() == 0) {
                decrypted_block.pop_back();
            }
        }
        
        result.insert(result.end(), decrypted_block.begin(), decrypted_block.end());
    }
    
    return std::string(result.begin(), result.end());
}
