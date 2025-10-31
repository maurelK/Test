/*
** EPITECH PROJECT, 2025
** GameEngine
** File description:
** The core of the Game Engine
*/

#ifndef GAME_ENGINE_HPP
#define GAME_ENGINE_HPP

#include "EngineConfig.hpp"
#include <memory>
#include "SceneManager.hpp"
#include "../ECS_architecture/Orchestror.hpp"
#include "../Resources/ResourceManager.hpp"

class GameEngine {
public:
    enum class Mode { 
        STANDALONE,  // Test/development
        CLIENT,      // Avec rendu
        SERVER       // Headless
    };

private:
    Mode currentMode;
    EngineConfig config;
    Orchestror orchestrator;
    std::unique_ptr<ResourceManager> resourceManager;
    std::unique_ptr<SceneManager> sceneManager;
    
    // SFML - seulement si Mode::CLIENT
    std::unique_ptr<sf::RenderWindow> window;

public:
    GameEngine();
    ~GameEngine();
    
    bool initialize(Mode mode, const EngineConfig& config);
    void run();
    void stop();
    
    Orchestror& getECS() { return orchestrator; }
    ResourceManager& getResources() { return *resourceManager; }
    SceneManager& getScenes() { return *sceneManager; }
    
    sf::RenderWindow* getWindow() { 
        return (currentMode == Mode::CLIENT) ? window.get() : nullptr; 
    }

private:
    void mainLoop();
    void processEvents();
    void update(float deltaTime);
    void render();
    
    void initClientMode();
    void initServerMode();
};

#endif
