#include "../../include/Core/GameEngine.hpp"
#include <iostream>
#include <chrono>
#include <thread>

GameEngine::GameEngine() 
    : currentMode(Mode::STANDALONE)
    , resourceManager(std::make_unique<ResourceManager>())
    , sceneManager(std::make_unique<SceneManager>()) {
}

GameEngine::~GameEngine() {
}

bool GameEngine::initialize(Mode mode, const EngineConfig& engineConfig)
{
    currentMode = mode;
    config = engineConfig;
    
    orchestrator.init();
    return true;
}

void GameEngine::run()
{
    mainLoop();
}

void GameEngine::stop()
{

}

void GameEngine::mainLoop() {
    if (currentMode == Mode::SERVER) {
        std::cout << "[GameEngine] Serveur en attente de clients..." << std::endl;
        // juste une attente passive
        std::cin.get(); // bloque ici jusqu'à ce que tu appuies sur Entrée
        // ou attendre un signal/condition variable de NetworkManager
    }
}

void GameEngine::processEvents() {
    std::cout << " ProcessEvents" << std::endl;
}

void GameEngine::update(float deltaTime) {
    std::cout << "Update - delta: " << deltaTime << std::endl;
    sceneManager->update(deltaTime);
    orchestrator.update(deltaTime);
}

/*void GameEngine::processEvents() {
    // Pas d'événements sans SFML
    static int eventCount = 0;
    if (eventCount++ % 60 == 0) {
        // std::cout << "Processing events..." << std::endl;
    }
}*/

/*void GameEngine::update(float deltaTime) {
    // Mettre à jour les scènes
    sceneManager->update(deltaTime);
    
    // Mettre à jour l'ECS
    orchestrator.update(deltaTime);
}*/

void GameEngine::render() {
    std::cout << " Render" << std::endl;
    
    sceneManager->render();
}

bool GameEngine::initClientMode() {
    std::cout << "  Mode CLIENT non disponible (SFML manquant)" << std::endl;
    return false; 
}

void GameEngine::initServerMode() {
    // Serveur headless - pas de fenêtre
    resourceManager.reset(); // Économie de mémoire
    std::cout << "Mode SERVEUR initialisé (headless)" << std::endl;
}
