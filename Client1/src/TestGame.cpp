#include "TestGame.hpp"
#include <iostream>

TestGame::TestGame() 
    : windowManager(800, 600, "Test - Background & PowerUps") {}

void TestGame::run() {
    if (!windowManager.createWindow()) {
        std::cout << "[TestGame] Erreur création fenêtre\n";
        return;
    }
    
    renderer.initBackground(100, windowManager.getWidth(), windowManager.getHeight());
    std::cout << "[TestGame] Background initialisé\n";
    
    while (windowManager.isOpen()) {
        float deltaTime = clock.restart().asSeconds();
        
        handleEvents();
        update(deltaTime);
        render();
    }
}

void TestGame::handleEvents() {
    sf::Event event;
    while (windowManager.pollEvent(event)) {
        if (event.type == sf::Event::Closed) {
            windowManager.closeWindow();
        }
        if (event.type == sf::Event::KeyPressed) {
            if (event.key.code == sf::Keyboard::Escape) {
                windowManager.closeWindow();
            }
        }
    }
}

void TestGame::update(float deltaTime) {
    renderer.updateBackground(deltaTime);
    
    // Spawn power-ups toutes les 2 secondes
    if (spawnClock.getElapsedTime().asSeconds() > 2.f) {
        spawnPowerUp();
        spawnClock.restart();
    }
    
    // Mise à jour des power-ups
    for (auto& powerUp : powerUps) {
        powerUp.update(deltaTime);
    }
    
    // Supprimer les power-ups hors écran
    powerUps.erase(
        std::remove_if(powerUps.begin(), powerUps.end(),
            [](const PowerUp& p) { return p.isOutOfBounds(); }),
        powerUps.end()
    );
}

void TestGame::render() {
    windowManager.clear(sf::Color(10, 10, 30));
    
    renderer.drawBackground(*windowManager.getWindow());
    renderer.drawPowerUps(*windowManager.getWindow(), powerUps);
    
    windowManager.display();
}

void TestGame::spawnPowerUp() {
    float y = 50.f + static_cast<float>(rand() % 500);
    PowerUpType type = static_cast<PowerUpType>(rand() % 3);
    powerUps.emplace_back(750.f, y, type);
    std::cout << "[TestGame] PowerUp spawné\n";
}
