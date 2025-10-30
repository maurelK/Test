/*
** EPITECH PROJECT, 2025
** ResourceManager.hpp
** File description:
** Gestion des Ressources
*/
#include <algorithm>
#include <unordered_map>
#include <memory>
#include <string>
#include <SFML/Graphics.hpp>
#include <SFML/Audio.hpp>

class ResourceManager
{
private:
    std::unordered_map<std::string, std::shared_ptr<sf::Texture>> textures;
    std::unordered_map<std::string, std::shared_ptr<sf::SoundBuffer>> sounds;
    std::unordered_map<std::string, std::shared_ptr<sf::Font>> fonts;

public:
    template<typename T>
    std::shared_ptr<T> load(const std::string& path);
    
    template<typename T>
    std::shared_ptr<T> get(const std::string& id);

    template<typename T>
    void unload(const std::string& id);

    void clear();
};
