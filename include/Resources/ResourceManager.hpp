/*
** EPITECH PROJECT, 2025
** ResourceManager.hpp
** File description:
** Gestion des Ressources
*/
/*
** EPITECH PROJECT, 2025
** ResourceManager.hpp
** File description:
** Gestion des Ressources (sans audio)
*/

#ifndef RESOURCES_MANAGER
#define RESOURCES_MANAGER

#include <algorithm>
#include <unordered_map>
#include <memory>
#include <string>
#include <iomanip>
#include <SFML/Graphics.hpp>
// #include <SFML/Audio.hpp>  // ⭐ RETIRÉ - pas disponible
#include <iostream>

class ResourceManager
{
private:
    std::unordered_map<std::string, std::shared_ptr<sf::Texture>> textures;
    // std::unordered_map<std::string, std::shared_ptr<sf::SoundBuffer>> sounds; // ⭐ RETIRÉ
    std::unordered_map<std::string, std::shared_ptr<sf::Font>> fonts;

public:
    ResourceManager() = default;
    ~ResourceManager() = default;

    // Charger une ressource (crée si n'existe pas)
    template<typename T>
    std::shared_ptr<T> load(const std::string& path);
    
    // Récupérer une ressource déjà chargée
    template<typename T>
    std::shared_ptr<T> get(const std::string& id);
    
    // Vérifier si une ressource est chargée
    template<typename T>
    bool isLoaded(const std::string& id) const;
    
    // Libérer une ressource spécifique
    template<typename T>
    void unload(const std::string& id);
    
    // Libérer toutes les ressources
    void clear();
    
    // Nettoyer les ressources non utilisées
    void garbageCollect();

private:
    // Méthodes internes spécialisées
    std::shared_ptr<sf::Texture> loadTexture(const std::string& path);
    // std::shared_ptr<sf::SoundBuffer> loadSound(const std::string& path); // ⭐ RETIRÉ
    std::shared_ptr<sf::Font> loadFont(const std::string& path);
};

// IMPLÉMENTATION DES TEMPLATES

template<typename T>
std::shared_ptr<T> ResourceManager::load(const std::string& path) {
    if constexpr (std::is_same_v<T, sf::Texture>) {
        return loadTexture(path);
    } 
    // else if constexpr (std::is_same_v<T, sf::SoundBuffer>) { // ⭐ RETIRÉ
    //     return loadSound(path);
    // } 
    else if constexpr (std::is_same_v<T, sf::Font>) {
        return loadFont(path);
    } else {
        static_assert(sizeof(T) == 0, "Unsupported resource type");
        return nullptr;
    }
}

template<typename T>
std::shared_ptr<T> ResourceManager::get(const std::string& id) {
    if constexpr (std::is_same_v<T, sf::Texture>) {
        auto it = textures.find(id);
        return (it != textures.end()) ? it->second : nullptr;
    } 
    // else if constexpr (std::is_same_v<T, sf::SoundBuffer>) { // ⭐ RETIRÉ
    //     auto it = sounds.find(id);
    //     return (it != sounds.end()) ? it->second : nullptr;
    // } 
    else if constexpr (std::is_same_v<T, sf::Font>) {
        auto it = fonts.find(id);
        return (it != fonts.end()) ? it->second : nullptr;
    } else {
        static_assert(sizeof(T) == 0, "Unsupported resource type");
        return nullptr;
    }
}

template<typename T>
bool ResourceManager::isLoaded(const std::string& id) const {
    if constexpr (std::is_same_v<T, sf::Texture>) {
        return textures.find(id) != textures.end();
    } 
    // else if constexpr (std::is_same_v<T, sf::SoundBuffer>) { // ⭐ RETIRÉ
    //     return sounds.find(id) != sounds.end();
    // } 
    else if constexpr (std::is_same_v<T, sf::Font>) {
        return fonts.find(id) != fonts.end();
    } else {
        static_assert(sizeof(T) == 0, "Unsupported resource type");
        return false;
    }
}

template<typename T>
void ResourceManager::unload(const std::string& id) {
    if constexpr (std::is_same_v<T, sf::Texture>) {
        textures.erase(id);
    } 
    // else if constexpr (std::is_same_v<T, sf::SoundBuffer>) { // ⭐ RETIRÉ
    //     sounds.erase(id);
    // } 
    else if constexpr (std::is_same_v<T, sf::Font>) {
        fonts.erase(id);
    } else {
        static_assert(sizeof(T) == 0, "Unsupported resource type");
    }
}

// IMPLÉMENTATION DES MÉTHODES INTERNES

inline std::shared_ptr<sf::Texture> ResourceManager::loadTexture(const std::string& path) {
    // Vérifier si déjà chargé
    auto it = textures.find(path);
    if (it != textures.end()) {
        std::cout << "Texture déjà chargée: " << path << std::endl;
        return it->second;
    }
    
    // Nouveau chargement
    auto texture = std::make_shared<sf::Texture>();
    if (!texture->loadFromFile(path)) {
        std::cerr << "ERREUR: Impossible de charger la texture: " << path << std::endl;
        return nullptr;
    }
    
    textures[path] = texture;
    std::cout << "Texture chargée: " << path << std::endl;
    return texture;
}

// ⭐ RETIRÉ - pas d'audio
/*
inline std::shared_ptr<sf::SoundBuffer> ResourceManager::loadSound(const std::string& path) {
    auto it = sounds.find(path);
    if (it != sounds.end()) {
        return it->second;
    }
    
    auto soundBuffer = std::make_shared<sf::SoundBuffer>();
    if (!soundBuffer->loadFromFile(path)) {
        std::cerr << "ERREUR: Impossible de charger le son: " << path << std::endl;
        return nullptr;
    }
    
    sounds[path] = soundBuffer;
    return soundBuffer;
}
*/

inline std::shared_ptr<sf::Font> ResourceManager::loadFont(const std::string& path) {
    auto it = fonts.find(path);
    if (it != fonts.end()) {
        return it->second;
    }
    
    auto font = std::make_shared<sf::Font>();
    if (!font->loadFromFile(path)) {
        std::cerr << "ERREUR: Impossible de charger la font: " << path << std::endl;
        return nullptr;
    }
    
    fonts[path] = font;
    return font;
}

inline void ResourceManager::clear() {
    textures.clear();
    // sounds.clear(); // ⭐ RETIRÉ
    fonts.clear();
}

inline void ResourceManager::garbageCollect() {
    // Textures
    for (auto it = textures.begin(); it != textures.end(); ) {
        if (it->second.use_count() == 1) {
            it = textures.erase(it);
        } else {
            ++it;
        }
    }
    
    // ⭐ RETIRÉ - pas d'audio
    /*
    // Sons
    for (auto it = sounds.begin(); it != sounds.end(); ) {
        if (it->second.use_count() == 1) {
            it = sounds.erase(it);
        } else {
            ++it;
        }
    }
    */
    
    // Fonts
    for (auto it = fonts.begin(); it != fonts.end(); ) {
        if (it->second.use_count() == 1) {
            it = fonts.erase(it);
        } else {
            ++it;
        }
    }
}

#endif