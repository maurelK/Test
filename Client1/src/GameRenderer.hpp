#ifndef GAMERENDERER_HPP
#define GAMERENDERER_HPP

#include <SFML/Graphics.hpp>
#include <vector>
#include "Background.hpp"
#include "game/PowerUp.hpp"

class GameRenderer {
private:
    std::vector<Star> stars;
    std::vector<Particle> particles;
    sf::Clock particleClock;

public:
    GameRenderer();
    
    void initBackground(unsigned int starCount, unsigned int windowWidth, unsigned int windowHeight);
    void updateBackground(float deltaTime);
    void drawBackground(sf::RenderTarget& target);
    
    void drawPowerUps(sf::RenderTarget& target, const std::vector<PowerUp>& powerUps);
    
    const std::vector<Star>& getStars() const { return stars; }
    std::vector<Particle>& getParticles() { return particles; }
};

#endif // GAMERENDERER_HPP
