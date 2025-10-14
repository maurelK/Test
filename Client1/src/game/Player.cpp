#include "Player.hpp"
#include <iostream>

Player::Player(float x, float y) {
    try {
        sprite.setTexture(TextureManager::get("assets/player.png"));
    } catch (const std::exception& e) {
        std::cerr << "[Player] Erreur chargement texture : " << e.what() << std::endl;
    }

    sprite.setScale(0.08f, 0.08f);
    sprite.setPosition(x, y);
}

void Player::update(float dt) {
    velocity = {0.f, 0.f};

    if (sf::Keyboard::isKeyPressed(sf::Keyboard::Up))
        velocity.y -= speed * dt;
    if (sf::Keyboard::isKeyPressed(sf::Keyboard::Down))
        velocity.y += speed * dt;
    if (sf::Keyboard::isKeyPressed(sf::Keyboard::Left))
        velocity.x -= speed * dt;
    if (sf::Keyboard::isKeyPressed(sf::Keyboard::Right))
        velocity.x += speed * dt;

    sprite.move(velocity);


    sf::Vector2f pos = sprite.getPosition();
    pos.x = std::clamp(pos.x, 0.f, 800.f - sprite.getGlobalBounds().width);
    pos.y = std::clamp(pos.y, 0.f, 600.f - sprite.getGlobalBounds().height);
    sprite.setPosition(pos);
}

void Player::draw(sf::RenderWindow& window) {
    window.draw(sprite);
}

Bullet Player::shoot() const {
    sf::Vector2f pos = sprite.getPosition();
    sf::FloatRect bounds = sprite.getGlobalBounds();
    return Bullet(pos.x + bounds.width, pos.y + bounds.height / 2.f);
}

sf::FloatRect Player::getBounds() const {
    return sprite.getGlobalBounds();
}

sf::Vector2f Player::getPosition() const {
    return sprite.getPosition();
}

void Player::setPosition(float x, float y) {
    sprite.setPosition(x, y);
}
