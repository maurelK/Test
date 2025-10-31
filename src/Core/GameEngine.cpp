#include "../../include/Core/GameEngine.hpp"
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
        if (!initClientMode()) {
            return false;
        }
    }
    // Mode serveur = headless
    else if (mode == Mode::SERVER) {
        initServerMode();
    }
    // Mode standalone = pas de SFML
    else if (mode == Mode::STANDALONE) {
        // Rien à initialiser pour SFML
    }
    
    return true;
}

void GameEngine::run() {
    mainLoop();
}

void GameEngine::stop() {
    if (window) {
        window->close();
    }
}

void GameEngine::mainLoop() {
    sf::Clock clock;
    bool running = true;
    
    while (running) {
        float deltaTime = clock.restart().asSeconds();
        
        processEvents();
        update(deltaTime);
        render();
        
        // Condition d'arrêt temporaire pour le test
        static int frameCount = 0;
        frameCount++;
        if (frameCount > 60) { // S'arrête après ~1 seconde à 60 FPS
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
        sceneManager->render();
        window->display();
    }
    // Mode standalone = pas de rendu
    else if (currentMode == Mode::STANDALONE) {
        // Juste un log pour le test
        static int renderCount = 0;
        if (renderCount++ % 60 == 0) {
            std::cout << "Standalone mode - no rendering" << std::endl;
        }
    }
}

bool GameEngine::initClientMode() {
    try {
        window = std::make_unique<sf::RenderWindow>(
            sf::VideoMode(config.window.width, config.window.height),
            config.window.title
        );
        window->setFramerateLimit(config.window.fpsLimit);
        return true;
    } catch (const std::exception& e) {
        std::cerr << "Failed to initialize SFML window: " << e.what() << std::endl;
        return false;
    }
}

void GameEngine::initServerMode() {
    // Serveur headless - pas de fenêtre
    resourceManager.reset(); // Économie de mémoire
}
