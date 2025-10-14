#ifndef BULLET_HPP
#define BULLET_HPP

#include <SFML/Graphics.hpp>
#include "TextureManager.hpp"

class Bullet {
private:
    sf::Sprite sprite;
    sf::CircleShape circle;
    float speed;
    float animationTime;
    bool fromPlayer;
    bool useSprite;

public:
    Bullet(float x, float y, bool fromPlayer = true);

    void update(float deltaTime);
    void draw(sf::RenderTarget& target);
    bool isOutOfBounds(float screenWidth) const;
    sf::FloatRect getBounds() const;
};

#endif
