#include "GameEngine.hpp"


/*GameEngine::GameEngine() 
    : currentState(State::UNINITIALIZED)
    , resourceManager(std::make_unique<ResourceManager>())
    , sceneManager(std::make_unique<SceneManager>()) {
}


bool GameEngine::initialize(Mode mode, const EngineConfig& config)
{
    currentMode = mode;
    if (mode == Mode::CLIENT) {
                // Initialiser SFML, ressources...
    } else if (mode == Mode::SERVER) {
        // Alb c'est ici on doit pouvoir gerer les bails du Server
    }

    orchestrator.init();
    return true;
}

void GameEngine::run() {
    if (currentState != State::INITIALIZED) return;
    
    currentState = State::RUNNING;
    mainLoop();
}

void GameEngine::mainLoop() {
    sf::Clock clock;
    
    while (currentState == State::RUNNING) {
        float deltaTime = clock.restart().asSeconds();
        
        processEvents();
        update(deltaTime);
        render();
    }
}

void GameEngine::processEvents() {
    // Gestion des événements SFML
}

void GameEngine::update(float deltaTime) {
    sceneManager->update(deltaTime);
    orchestrator.update(deltaTime);
}

void GameEngine::render() {
    //  Rendu SFML
    //sceneManager->render();
}*/