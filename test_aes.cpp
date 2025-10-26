#include <iostream>
#include <vector>
#include <string>
#include <sstream>
#include <iomanip>

std::vector<unsigned char> message_to_32bit_numbers(const std::string& message) {
    std::vector<unsigned char> msg_bytes(message.begin(), message.end());
    
    // Pad to exactly 16 bytes with zeros
    while (msg_bytes.size() < 16) {
        msg_bytes.push_back(0);
    }
    
    return msg_bytes;
}

std::string convert_bytes_to_32bit_numbers(const std::vector<unsigned char>& bytes) {
    std::stringstream result;
    
    for (int i = 0; i < 4; i++) {
        uint32_t number = 0;
        for (int j = 0; j < 4; j++) {
            number |= (bytes[i * 4 + j] << (8 * j));
        }
        
        std::stringstream ss;
        ss << std::hex << std::setw(8) << std::setfill('0') << number;
        result << ss.str();
    }
    
    return result.str();
}

int main() {
    std::string message = "All men must die";
    
    auto msg_bytes = message_to_32bit_numbers(message);
    std::string numbers_hex = convert_bytes_to_32bit_numbers(msg_bytes);
    
    std::cout << "Message: '" << message << "'" << std::endl;
    std::cout << "Message as 32-bit numbers: " << numbers_hex << std::endl;
    
    return 0;
}