#include "Bullet.hpp"

Bullet::Bullet(float x, float y, bool fromPlayer) 
    : speed(fromPlayer ? 600.f : -400.f), fromPlayer(fromPlayer) {
    
    shape.setSize(sf::Vector2f(20, 8));
    shape.setPosition(x, y);
    shape.setFillColor(fromPlayer ? sf::Color::Yellow : sf::Color::Red);
}

void Bullet::update(float deltaTime) {
    shape.move(speed * deltaTime, 0);
}

void Bullet::draw(sf::RenderTarget& target) {
    target.draw(shape);
}

bool Bullet::isOutOfBounds(float screenWidth) const {
    return shape.getPosition().x > screenWidth || shape.getPosition().x < -20;
}

sf::FloatRect Bullet::getBounds() const {
    return shape.getGlobalBounds();
}