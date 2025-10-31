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
    
    // Pour l'instant, tous les modes sont identiques (sans SFML)
    std::cout << "Engine initialisé en mode: ";
    switch(mode) {
        case Mode::STANDALONE: std::cout << "STANDALONE"; break;
        case Mode::CLIENT: std::cout << "CLIENT (sans SFML)"; break;
        case Mode::SERVER: std::cout << "SERVER"; break;
    }
    std::cout << std::endl;
    
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
    std::cout << "Démarrage de la boucle principale SIMPLIFIÉE..." << std::endl;
    
    for (int frame = 0; frame < 10; frame++) {
        std::cout << "=== FRAME " << frame << " ===" << std::endl;
        
        processEvents();
        update(0.016f);
        render();
        std::this_thread::sleep_for(std::chrono::milliseconds(100));
    }
    
    std::cout << " Boucle principale terminée" << std::endl;
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
