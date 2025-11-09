/*
** EPITECH PROJECT, 2025
** Gestion des Assets (sans audio)
** File description:
** Assets
*/

#include <memory>
#include <SFML/Graphics.hpp>
// #include <SFML/Audio.hpp>

class AssetFactory {
public:
    static std::shared_ptr<sf::Texture> createTexture(const std::string& path);
    static std::shared_ptr<sf::Font> createFont(const std::string& path);
};