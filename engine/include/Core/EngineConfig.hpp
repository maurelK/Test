/*
** EPITECH PROJECT, 2025
** EngineConfig
** File description:
** Basic Engine Configuration (sans audio)
*/
#include <algorithm>
#include <iomanip>
#include <string>

struct EngineConfig {
    struct WindowConfig {
        std::string title;
        uint32_t width;
        uint32_t height;
        uint32_t fpsLimit;
        bool fullscreen;
    } window;
    
    struct PhysicsConfig {
        float gravity;
        uint32_t velocityIterations;
        uint32_t positionIterations;
    } physics;
    
    //  RETIRÉ - pas d'audio pour le moment
    /*
    struct AudioConfig {
        float masterVolume;
        float musicVolume;
        float sfxVolume;
    } audio;
    */
    
    struct ECSConfig {
        uint32_t maxEntities;
        uint32_t maxComponents;
    } ecs;
};
