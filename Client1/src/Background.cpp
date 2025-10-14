#include "Background.hpp"
#include <cmath>
#include <cstdlib>

float gridOffsetX = 0.f;
float gridOffsetY = 0.f;
float gridSpeed = 2.5f;

Star::Star(float x, float y, float size) {
    shape.setPosition(x, y);
    shape.setRadius(size);
    shape.setFillColor(sf::Color::White);
    shape.setPointCount(4);
    shape.setOrigin(size / 2, size / 2);
    delay = static_cast<float>(rand() % 3000) / 1000.f;
}

void Star::update(float deltaTime) {
    animationTime += deltaTime;
    float phase = (animationTime + delay) * twinkleSpeed;
    float opacity = 0.3f + 0.7f * (0.5f + 0.5f * sin(phase * 2 * 3.14159f));
    float scale = 1.f + 0.2f * (0.5f + 0.5f * sin(phase * 2 * 3.14159f));
    shape.setScale(scale, scale);
    sf::Color color(255, 255, 255, static_cast<sf::Uint8>(opacity * 255));
    shape.setFillColor(color);
}

void Star::draw(sf::RenderTarget &target) const {
    target.draw(shape);
}

Particle::Particle(float x) {
    shape.setPosition(x, 720.f);
    shape.setRadius(1.f);
    shape.setFillColor(sf::Color(16, 33, 62));
    shape.setOrigin(1.f, 1.f);
    speed = 50.f + static_cast<float>(rand() % 50);
    driftX = static_cast<float>(rand() % 200 - 100);
}

bool Particle::update(float deltaTime) {
    lifetime += deltaTime;
    if (lifetime > maxLifetime)
        return false;

    shape.move(driftX * deltaTime, -speed * deltaTime);
    float progress = lifetime / maxLifetime;
    float opacity = 1.f - progress;
    shape.setFillColor(
        sf::Color(0, 255, 255, static_cast<sf::Uint8>(opacity * 255)));
    return true;
}

void Particle::draw(sf::RenderTarget &target) const {
    target.draw(shape);
}

void createStars(std::vector<Star> &stars, unsigned int count,
                 const sf::RenderWindow &window) {
    stars.clear();
    for (unsigned int i = 0; i < count; ++i) {
        float x = static_cast<float>(rand() % static_cast<int>(window.getSize().x));
        float y = static_cast<float>(rand() % static_cast<int>(window.getSize().y));
        float size = 1.f + static_cast<float>(rand() % 3);
        stars.emplace_back(x, y, size);
    }
}

void updateParticles(std::vector<Particle> &particles, sf::Clock &particleClock,
                     float deltaTime) {
    particles.erase(
        std::remove_if(particles.begin(), particles.end(),
                       [&](Particle &p) { return !p.update(deltaTime); }),
        particles.end());

    if (particleClock.getElapsedTime().asSeconds() > 2.f && particles.size() < 20) {
        float x = static_cast<float>(rand() % 1280);
        particles.emplace_back(x);
        particleClock.restart();
    }
}

void drawGrid(sf::RenderTarget &target, float offsetX, float offsetY, float gridSize) {
    sf::VertexArray grid(sf::Lines);
    sf::Color gridColor(0, 255, 255, 10);
    sf::Vector2u size = target.getSize();

    for (float x = offsetX; x <= static_cast<float>(size.x); x += gridSize) {
        grid.append(sf::Vertex(sf::Vector2f(x, offsetY), gridColor));
        grid.append(sf::Vertex(sf::Vector2f(x, static_cast<float>(size.y) + offsetY), gridColor));
    }

    for (float y = offsetY; y <= static_cast<float>(size.y); y += gridSize) {
        grid.append(sf::Vertex(sf::Vector2f(offsetX, y), gridColor));
        grid.append(sf::Vertex(sf::Vector2f(static_cast<float>(size.x) + offsetX, y), gridColor));
    }

    target.draw(grid);
}