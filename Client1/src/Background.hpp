#ifndef BACKGROUND_HPP
#define BACKGROUND_HPP

#include <SFML/Graphics.hpp>
#include <vector>

// Classe simple pour une étoile scintillante
class Star {
  public:
    sf::CircleShape shape;
    float animationTime = 0.f;
    float twinkleSpeed = 3.f;
    float delay = 0.f;

    Star(float x, float y, float size);
    void update(float deltaTime);
    void draw(sf::RenderTarget &target) const;
};

// Classe simple pour une particule flottante
class Particle {
  public:
    sf::CircleShape shape;
    float speed = 0.f;
    float lifetime = 0.f;
    float maxLifetime = 20.f;
    float driftX = 0.f;

    Particle(float x);
    bool update(float deltaTime);
    void draw(sf::RenderTarget &target) const;
};

// Fonctions pour le background (chacune <15 lignes)
void createStars(std::vector<Star> &stars, unsigned int count,
                 const sf::RenderWindow &window);
void updateParticles(std::vector<Particle> &particles, sf::Clock &particleClock,
                     float deltaTime);
void drawGrid(sf::RenderTarget &target, float offsetX, float offsetY,
              float gridSize = 50.f);

// Variables globales pour grille (simples)
extern float gridOffsetX;
extern float gridOffsetY;
extern float gridSpeed;

#endif // BACKGROUND_HPP