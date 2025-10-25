/*
** EPITECH PROJECT, 2024
** my_pgp
** File description:
** rsa_cipher
*/

#include "rsa_cipher.hpp"
#include "error_handler.hpp"
#include <sstream>
#include <iomanip>

RsaCipher::RsaCipher() {}

mpz_class RsaCipher::compute_n(const mpz_class& p, const mpz_class& q)
{
    return p * q;
}

mpz_class RsaCipher::gcd(mpz_class a, mpz_class b)
{
    while (b != 0) {
        mpz_class temp = b;
        b = a % b;
        a = temp;
    }
    return a;
}

mpz_class RsaCipher::lcm(const mpz_class& a, const mpz_class& b)
{
    return (a * b) / gcd(a, b);
}

mpz_class RsaCipher::compute_lambda(const mpz_class& p, const mpz_class& q)
{
    return lcm(p - 1, q - 1);
}

mpz_class RsaCipher::find_public_exponent(const mpz_class& lambda)
{
    // Try Fermat primes in order: 65537, 257, 17, 5, 3
    // 65537 (0x10001) is the standard RSA public exponent
    mpz_class fermat_primes[] = {65537, 257, 17, 5, 3};
    
    for (int i = 0; i < 5; i++) {
        if (fermat_primes[i] < lambda && gcd(fermat_primes[i], lambda) == 1)
            return fermat_primes[i];
    }
    Error::exit_with_error("Cannot find valid public exponent");
    return 0;
}

mpz_class RsaCipher::compute_private_exponent(
    const mpz_class& e,
    const mpz_class& lambda)
{
    mpz_class d;
    mpz_invert(d.get_mpz_t(), e.get_mpz_t(), lambda.get_mpz_t());
    return d;
}

std::string RsaCipher::format_key(const mpz_class& exp, const mpz_class& n)
{
    std::stringstream ss_exp, ss_n;
    ss_exp << std::hex << exp;
    ss_n << std::hex << n;
    
    std::string exp_str = ss_exp.str();
    std::string n_str = ss_n.str();
    
    // Ensure even number of hex digits
    if (exp_str.length() % 2 != 0)
        exp_str = "0" + exp_str;
    if (n_str.length() % 2 != 0)
        n_str = "0" + n_str;
    
    // Convert to little-endian by reversing byte pairs
    std::string exp_le, n_le;
    for (size_t i = exp_str.length(); i > 0; i -= 2)
        exp_le += exp_str.substr(i - 2, 2);
    for (size_t i = n_str.length(); i > 0; i -= 2)
        n_le += n_str.substr(i - 2, 2);
    
    return exp_le + "-" + n_le;
}

void RsaCipher::parse_key(const std::string& key, mpz_class& exp, mpz_class& n)
{
    size_t pos = key.find('-');
    if (pos == std::string::npos)
        Error::exit_with_error("Invalid key format");
    
    std::string exp_le = key.substr(0, pos);
    std::string n_le = key.substr(pos + 1);
    
    // Convert from little-endian to big-endian by reversing byte pairs
    std::string exp_be, n_be;
    for (size_t i = exp_le.length(); i > 0; i -= 2)
        exp_be += exp_le.substr(i - 2, 2);
    for (size_t i = n_le.length(); i > 0; i -= 2)
        n_be += n_le.substr(i - 2, 2);
    
    exp.set_str(exp_be, 16);
    n.set_str(n_be, 16);
}

mpz_class RsaCipher::message_to_number(const std::string& message)
{
    mpz_class result = 0;
    
    for (size_t i = 0; i < message.size(); i++) {
        mpz_class byte = static_cast<unsigned char>(message[i]);
        result += byte << (8 * i);
    }
    return result;
}

std::string RsaCipher::number_to_message(const mpz_class& num)
{
    if (num == 0)
        return "";
    
    std::stringstream ss;
    ss << std::hex << num;
    std::string hex_str = ss.str();
    
    if (hex_str.length() % 2 != 0)
        hex_str = "0" + hex_str;
    
    std::string result;
    for (size_t i = hex_str.length(); i > 0; i -= 2) {
        std::string byte_str = hex_str.substr(i - 2, 2);
        unsigned char byte = std::stoi(byte_str, nullptr, 16);
        result += static_cast<char>(byte);
    }
    return result;
}

RsaKeys RsaCipher::generate_keys(const std::string& p_str, const std::string& q_str)
{
    // Convert primes from little-endian to big-endian
    std::string p_be, q_be;
    for (size_t i = p_str.length(); i > 0; i -= 2)
        p_be += p_str.substr(i - 2, 2);
    for (size_t i = q_str.length(); i > 0; i -= 2)
        q_be += q_str.substr(i - 2, 2);
    
    mpz_class p(p_be, 16);
    mpz_class q(q_be, 16);
    mpz_class n = compute_n(p, q);
    mpz_class lambda = compute_lambda(p, q);
    mpz_class e = find_public_exponent(lambda);
    mpz_class d = compute_private_exponent(e, lambda);
    
    RsaKeys keys;
    keys.public_key = format_key(e, n);
    keys.private_key = format_key(d, n);
    return keys;
}

std::string RsaCipher::cipher(const std::string& message, const std::string& public_key)
{
    mpz_class e, n;
    parse_key(public_key, e, n);
    mpz_class m = message_to_number(message);
    mpz_class c;
    mpz_powm(c.get_mpz_t(), m.get_mpz_t(), e.get_mpz_t(), n.get_mpz_t());
    
    std::stringstream ss;
    ss << std::hex << c;
    std::string result = ss.str();
    
    // Ensure even number of hex digits
    if (result.length() % 2 != 0)
        result = "0" + result;
    
    // Convert to little-endian by reversing byte pairs
    std::string result_le;
    for (size_t i = result.length(); i > 0; i -= 2)
        result_le += result.substr(i - 2, 2);
    
    return result_le;
}

std::string RsaCipher::decipher(const std::string& cipher_text, const std::string& private_key)
{
    mpz_class d, n;
    parse_key(private_key, d, n);
    
    // Convert from little-endian to big-endian
    std::string cipher_be;
    for (size_t i = cipher_text.length(); i > 0; i -= 2)
        cipher_be += cipher_text.substr(i - 2, 2);
    
    mpz_class c(cipher_be, 16);
    mpz_class m;
    mpz_powm(m.get_mpz_t(), c.get_mpz_t(), d.get_mpz_t(), n.get_mpz_t());
    
    return number_to_message(m);
}
