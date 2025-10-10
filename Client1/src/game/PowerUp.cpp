#include "PowerUp.hpp"

PowerUp::PowerUp(float x, float y, PowerUpType type) 
    : type(type), speed(-100.f) {
    
    shape.setRadius(15);
    shape.setPosition(x, y);
    shape.setOrigin(15, 15);
    
    switch(type) {
        case PowerUpType::Life:
            shape.setFillColor(sf::Color::Green);
            break;
        case PowerUpType::Score:
            shape.setFillColor(sf::Color(255, 215, 0));
            break;
        case PowerUpType::Weapon:
            shape.setFillColor(sf::Color::Cyan);
            break;
    }
}

void PowerUp::update(float deltaTime) {
    shape.move(speed * deltaTime, 0);
}

void PowerUp::draw(sf::RenderTarget& target) const {
    target.draw(shape);
}

sf::FloatRect PowerUp::getBounds() const {
    return shape.getGlobalBounds();
}

PowerUpType PowerUp::getType() const {
    return type;
}

bool PowerUp::isOutOfBounds() const {
    return shape.getPosition().x < -30;
}