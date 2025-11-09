#ifndef POWERUP_HPP
#define POWERUP_HPP

#include <SFML/Graphics.hpp>

enum class PowerUpType {
    Life, Score, Weapon
};

class PowerUp {
private:
    sf::CircleShape shape;
    sf::CircleShape glowShape;
    PowerUpType type;
    float speed;
    float animationTime;
    float pulseSpeed;
    float rotationSpeed;
    bool collected;
    
public:
    PowerUp(float x, float y, PowerUpType type);
    void update(float deltaTime);
    void draw(sf::RenderTarget& target) const;
    sf::FloatRect getBounds() const;
    PowerUpType getType() const;
    bool isOutOfBounds() const;
    bool isCollected() const { return collected; }
    void collect() { collected = true; }
};

#endif