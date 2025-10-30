/*
** EPITECH PROJECT, 2025
** Geestion des Assets
** File description:
** Assets
*/

#include <memory>

class AssetFactory {
public:
    static std::shared_ptr<ITexture> createTexture(const std::string& path);
    static std::shared_ptr<ISoundBuffer> createSound(const std::string& path);
    static std::shared_ptr<IFont> createFont(const std::string& path);
};