#pragma once
#include <SFML/Graphics.hpp>
#include <map>
#include <string>

class TextureManager {
public:
    static sf::Texture& get(const std::string& path) {
        static std::map<std::string, sf::Texture> textures;
        auto it = textures.find(path);
        if (it == textures.end()) {
            sf::Texture tex;
            if (!tex.loadFromFile(path)) {
                throw std::runtime_error("Erreur chargement texture : " + path);
            }
            textures[path] = std::move(tex);
        }
        return textures[path];
    }
};
