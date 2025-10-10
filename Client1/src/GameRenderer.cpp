#include "GameRenderer.hpp"
#include <cstdlib>

GameRenderer::GameRenderer() {}

void GameRenderer::initBackground(unsigned int starCount, unsigned int windowWidth, unsigned int windowHeight) {
    stars.clear();
    for (unsigned int i = 0; i < starCount; ++i) {
        float x = static_cast<float>(rand() % windowWidth);
        float y = static_cast<float>(rand() % windowHeight);
        float size = 1.f + static_cast<float>(rand() % 3);
        stars.emplace_back(x, y, size);
    }
}

void GameRenderer::updateBackground(float deltaTime) {
    // Mise à jour des étoiles
    for (auto& star : stars) {
        star.update(deltaTime);
    }
    
    // Mise à jour des particules
    updateParticles(particles, particleClock, deltaTime);
    
    // Mise à jour de la grille (défilement)
    gridOffsetX -= gridSpeed;
    if (gridOffsetX <= -50.f) {
        gridOffsetX = 0.f;
    }
}

void GameRenderer::drawBackground(sf::RenderTarget& target) {
    // Dessiner la grille
    drawGrid(target, gridOffsetX, gridOffsetY);
    
    // Dessiner les étoiles
    for (const auto& star : stars) {
        star.draw(target);
    }
    
    // Dessiner les particules
    for (const auto& particle : particles) {
        particle.draw(target);
    }
}

void GameRenderer::drawPowerUps(sf::RenderTarget& target, const std::vector<PowerUp>& powerUps) {
    for (const auto& powerUp : powerUps) {
        powerUp.draw(target);
    }
}
