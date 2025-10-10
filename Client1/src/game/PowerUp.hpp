#ifndef POWERUP_HPP
#define POWERUP_HPP

#include <SFML/Graphics.hpp>

enum class PowerUpType {
    Life, Score, Weapon
};

class PowerUp {
private:
    sf::CircleShape shape;
    PowerUpType type;
    float speed;
    
public:
    PowerUp(float x, float y, PowerUpType type);
    void update(float deltaTime);
    void draw(sf::RenderTarget& target) const;
    sf::FloatRect getBounds() const;
    PowerUpType getType() const;
    bool isOutOfBounds() const;
};

#endif