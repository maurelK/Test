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
    enum class State { UNINITIALIZED, INITIALIZED, RUNNING, PAUSED, STOPPED };
    enum class Mode { SERVER, CLIENT, STANDALONE };

private:
    State currentState;
    Orchestror orchestrator;
    std::unique_ptr<ResourceManager> resourceManager;
    std::unique_ptr<SceneManager> sceneManager;
    
    // La Configuration partie 
    EngineConfig config;
    Mode currentMode;
    
public:
    GameEngine();
    ~GameEngine();
    
    // Cycle de vie
    bool initialize(Mode mode, const EngineConfig& config);
    void run();
    void pause();
    void resume();
    void shutdown();
    
    // Gestion des scènes
    void pushScene(std::unique_ptr<Scene> scene);
    void popScene();
    
    // Accès aux sous-systèmes
    Orchestror& getECS()
    {
        return orchestrator;
    }
    ResourceManager& getResourceManager()
    {
        return *resourceManager;
    }
    SceneManager& getSceneManager(){
        return *sceneManager;
    }
    

private:
    void mainLoop();
    void processEvents();
    void update(float deltaTime);
    void render();
};

#endif
