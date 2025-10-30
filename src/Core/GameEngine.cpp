#include "GameEngine.hpp"


bool GameEngine::initialize(const std::string &title, int width, int height)
{
    orchestrator.init();
    window.create(sf::VideoMode(width, height), title);        
    return true;
}

void GameEngine::run()
{
    isRunning = true;
    sf::Clock clock;
    
    while (isRunning && window.isOpen()) {
        float dt = clock.restart().asSeconds();
        
        handleEvents();
        update(dt);
        render();
    }
}

void GameEngine::stop()
{
    isRunning = false;
}