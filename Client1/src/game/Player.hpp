#ifndef PLAYER_HPP
#define PLAYER_HPP

#include <SFML/Graphics.hpp>
#include "Bullet.hpp"

class Player {
private:
    sf::RectangleShape shape;  // Fallback si pas de texture
    sf::Sprite sprite;         // Sprite principal
    sf::Texture texture;
    float speed;
    bool usingTexture = false; // Flag pour savoir si on utilise le sprite
    
public:
    Player(float x, float y);
    void update(float deltaTime);
    void draw(sf::RenderTarget& target);
    Bullet shoot();
    sf::FloatRect getBounds() const;
    sf::Vector2f getPosition() const;
};

#endif