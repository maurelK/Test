#include "Player.hpp"
#include <iostream>

Player::Player(float x, float y) : speed(400.f) {
    std::cout << "Tentative de chargement de assets/player.png..." << std::endl;
    
    if (!texture.loadFromFile("assets/player.png")) {
        std::cout << "ERREUR: Impossible de charger assets/player.png" << std::endl;
        std::cout << "Vérifiez que le fichier existe et que vous lancez le jeu depuis Client1/" << std::endl;
        // Fallback rectangle vert
        shape.setSize(sf::Vector2f(70, 70));
        shape.setFillColor(sf::Color::Green);
        usingTexture = false;
    } else {
        std::cout << "SUCCESS: Sprite chargé avec succès!" << std::endl;
        sprite.setTexture(texture);
        
        // Redimensionner si nécessaire
        sf::Vector2u textureSize = texture.getSize();
        std::cout << "Taille du sprite: " << textureSize.x << "x" << textureSize.y << std::endl;
        
        float scaleX = 70.0f / textureSize.x;
        float scaleY = 50.0f / textureSize.y;
        sprite.setScale(scaleX, scaleY);
        
        usingTexture = true;
    }
    
    shape.setPosition(x, y);
    sprite.setPosition(x, y);
}

void Player::update(float deltaTime) {
    sf::Vector2f movement(0, 0);
    
    if (sf::Keyboard::isKeyPressed(sf::Keyboard::Up) && getPosition().y > 0) {
        movement.y = -speed * deltaTime;
    }
    if (sf::Keyboard::isKeyPressed(sf::Keyboard::Down) && getPosition().y < 680) {
        movement.y = speed * deltaTime;
    }
    if (sf::Keyboard::isKeyPressed(sf::Keyboard::Left) && getPosition().x > 0) {
        movement.x = -speed * deltaTime;
    }
    if (sf::Keyboard::isKeyPressed(sf::Keyboard::Right) && getPosition().x < 400) {
        movement.x = speed * deltaTime;
    }
    
    shape.move(movement);
    sprite.move(movement);
}

void Player::draw(sf::RenderTarget& target) {
    if (usingTexture) {
        target.draw(sprite);
    } else {
        target.draw(shape);
    }
}

Bullet Player::shoot() {
    sf::Vector2f pos = getPosition();
    return Bullet(pos.x + 60, pos.y + 15, true);
}

sf::FloatRect Player::getBounds() const {
    if (usingTexture) {
        return sprite.getGlobalBounds();
    } else {
        return shape.getGlobalBounds();
    }
}

sf::Vector2f Player::getPosition() const {
    if (usingTexture) {
        return sprite.getPosition();
    } else {
        return shape.getPosition();
    }
}