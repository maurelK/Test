/*
** EPITECH PROJECT, 2025
** ResourceManager.hpp
** File description:
** Gestion des Ressources (sans SFML)
*/

#ifndef RESOURCES_MANAGER
#define RESOURCES_MANAGER

#include <algorithm>
#include <unordered_map>
#include <memory>
#include <string>
#include <iostream>

//  - pas de SFML
// #include <SFML/Graphics.hpp>

class ResourceManager
{
private:
    //  RETIRÉ - pas de SFML
    // std::unordered_map<std::string, std::shared_ptr<sf::Texture>> textures;
    // std::unordered_map<std::string, std::shared_ptr<sf::Font>> fonts;

public:
    ResourceManager() = default;
    ~ResourceManager() = default;

    // Pour l'instant, gestion basique sans SFML
    template<typename T>
    std::shared_ptr<T> load(const std::string& path) {
        std::cout << "  ResourceManager: Chargement non disponible sans SFML - " << path << std::endl;
        return nullptr;
    }
    
    template<typename T>
    std::shared_ptr<T> get(const std::string& id) {
        return nullptr;
    }
    
    template<typename T>
    bool isLoaded(const std::string& id) const {
        return false;
    }
    
    template<typename T>
    void unload(const std::string& id) {
    }
    
    void clear() {
        std::cout << "ResourceManager: clear()" << std::endl;
    }
    
    void garbageCollect() {
        std::cout << "ResourceManager: garbageCollect()" << std::endl;
    }
};

#endif