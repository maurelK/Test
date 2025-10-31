#include "../../include/Core/GameEngine.hpp"

#include <SFML/Graphics.hpp>

GameEngine::GameEngine() 
    : currentMode(Mode::STANDALONE)
    , resourceManager(std::make_unique<ResourceManager>())
    , sceneManager(std::make_unique<SceneManager>()) {
}

GameEngine::~GameEngine() {
    // Cleanup
}

bool GameEngine::initialize(Mode mode, const EngineConfig& engineConfig) {
    currentMode = mode;
    config = engineConfig;
    
    // Initialiser l'ECS
    orchestrator.init();
    
    // Mode client = initialiser SFML
    if (mode == Mode::CLIENT) {
        initClientMode();
    }
    // Mode serveur = headless
    else if (mode == Mode::SERVER) {
        initServerMode();
    }
    
    return true;
}

void GameEngine::run() {
    mainLoop();
}

void GameEngine::stop() {
    // À implémenter
}

void GameEngine::mainLoop() {
    sf::Clock clock;
    bool running = true;
    
    while (running) {
        float deltaTime = clock.restart().asSeconds();
        
        processEvents();
        update(deltaTime);
        render();
        
        // Condition d'arrêt temporaire
        if (deltaTime > 1.0f) { // Juste pour le test
            running = false;
        }
    }
}

void GameEngine::processEvents() {
    // Mode client seulement
    if (currentMode == Mode::CLIENT && window) {
        sf::Event event;
        while (window->pollEvent(event)) {
            if (event.type == sf::Event::Closed) {
                stop();
            }
        }
    }
}

void GameEngine::update(float deltaTime) {
    // Mettre à jour les scènes
    sceneManager->update(deltaTime);
    
    // Mettre à jour l'ECS
    orchestrator.update(deltaTime);
}

void GameEngine::render() {
    // Mode client seulement
    if (currentMode == Mode::CLIENT && window) {
        window->clear(sf::Color::Black);
        // Rendu des scènes...
        window->display();
    }
}

void GameEngine::initClientMode() {
    window = std::make_unique<sf::RenderWindow>(
        sf::VideoMode(config.window.width, config.window.height),
        config.window.title
    );
    window->setFramerateLimit(config.window.fpsLimit);
}

void GameEngine::initServerMode() {
    // Serveur headless - pas de fenêtre
    resourceManager.reset(); // Économie de mémoire
}