/*
** EPITECH PROJECT, 2024
** my_pgp
** File description:
** rsa_cipher
*/

#ifndef RSA_CIPHER_HPP_
#define RSA_CIPHER_HPP_

#include <string>
#include <gmpxx.h>

struct RsaKeys {
    std::string public_key;
    std::string private_key;
};

class RsaCipher {
public:
    RsaCipher();
    RsaKeys generate_keys(const std::string& p_str, const std::string& q_str);
    std::string cipher(const std::string& message, const std::string& public_key);
    std::string decipher(const std::string& cipher_text, const std::string& private_key);

private:
    mpz_class compute_n(const mpz_class& p, const mpz_class& q);
    mpz_class gcd(mpz_class a, mpz_class b);
    mpz_class lcm(const mpz_class& a, const mpz_class& b);
    mpz_class compute_lambda(const mpz_class& p, const mpz_class& q);
    mpz_class find_public_exponent(const mpz_class& lambda);
    mpz_class compute_private_exponent(const mpz_class& e, const mpz_class& lambda);
    
    std::string format_key(const mpz_class& exp, const mpz_class& n);
    void parse_key(const std::string& key, mpz_class& exp, mpz_class& n);
    
    mpz_class message_to_number(const std::string& message);
    std::string number_to_message(const mpz_class& num);
};

#endif /* !RSA_CIPHER_HPP_ */
