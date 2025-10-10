#ifndef BULLET_HPP
#define BULLET_HPP

#include <SFML/Graphics.hpp>

class Bullet {
private:
    sf::RectangleShape shape;
    float speed;
    bool fromPlayer;
    
public:
    Bullet(float x, float y, bool fromPlayer);
    void update(float deltaTime);
    void draw(sf::RenderTarget& target);
    bool isOutOfBounds(float screenWidth) const;
    sf::FloatRect getBounds() const;
};

#endif