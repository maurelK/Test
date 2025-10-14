#pragma once
#include <SFML/Graphics.hpp>
#include "TextureManager.hpp"
#include "Bullet.hpp"

class Player {
public:
    Player(float x = 200.f, float y = 360.f);

    void update(float dt);
    void draw(sf::RenderWindow& window);

    Bullet shoot() const;
    sf::FloatRect getBounds() const;
    sf::Vector2f getPosition() const;
    void setPosition(float x, float y);

private:
    sf::Sprite sprite;
    sf::Vector2f velocity;
    float speed = 300.f;
};
