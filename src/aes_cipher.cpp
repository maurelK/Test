#include "aes_cipher.hpp"
#include <cstdint>

AesCipher::AesCipher(const std::string &key)
{
}
std::array<std::array<uint8_t, 4>, 4> AesCipher::AddRoundKey_operation(const uint8_t state[4][4], const uint8_t key[4][4])
{
    std::array<std::array<uint8_t, 4>, 4> result;
    for (int i = 0; i < 4; ++i) {
        for (int j = 0; j < 4; ++j) {
            result[i][j] = state[i][j] ^ key[i][j];
        }
    }
    return result;
}

u_int32_t AesCipher::SubWord(u_int32_t word)
{
    u_int8_t bytes;
    uint8_t subword;
    uint32_t result = 0;
    
    for (int i = 0; i <4 ; i++) {
        bytes = (word >> (i * 8)) & 0xFF;
        subword = sBox[bytes];
        result |= (subword << (i * 8));
    }
    return result;
}

void AesCipher::SubBytes(uint8_t state[4][4])
{
    for (int i = 0; i < 4; ++i) {
        for (int j = 0; j < 4; ++j) {
            state[i][j] = sBox[state[i][j]];
        }
    }
}

u_int32_t AesCipher::RotWord(u_int32_t word)
{
    return ((word << 8) | (word >> 24));
}

void AesCipher::ShiftRows(uint8_t state[4][4])
{
    uint8_t temp;

    temp = state[1][0];
    for (int i = 0; i < 3; ++i) state[1][i] = state[1][i+1];
    state[1][3] = temp;

    uint8_t t0 = state[2][0], t1 = state[2][1];
    state[2][0] = state[2][2]; state[2][1] = state[2][3];
    state[2][2] = t0; state[2][3] = t1;

    temp = state[3][3];
    for (int i = 3; i > 0; --i) state[3][i] = state[3][i-1];
    state[3][0] = temp;
}
