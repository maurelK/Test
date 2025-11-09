#include "PowerUp.hpp"
#include <cmath>

PowerUp::PowerUp(float x, float y, PowerUpType type) 
    : type(type), speed(-100.f), animationTime(0.f), collected(false) {
    
    shape.setRadius(15);
    shape.setPosition(x, y);
    shape.setOrigin(15, 15);
    
    glowShape.setRadius(20);
    glowShape.setPosition(x, y);
    glowShape.setOrigin(20, 20);
    
    switch(type) {
        case PowerUpType::Life:
            shape.setFillColor(sf::Color::Green);
            glowShape.setFillColor(sf::Color(0, 255, 0, 60));
            pulseSpeed = 2.5f;
            rotationSpeed = 90.f;
            break;
        case PowerUpType::Score:
            shape.setFillColor(sf::Color(255, 215, 0));
            glowShape.setFillColor(sf::Color(255, 215, 0, 60));
            pulseSpeed = 3.0f;
            rotationSpeed = 120.f;
            break;
        case PowerUpType::Weapon:
            shape.setFillColor(sf::Color::Cyan);
            glowShape.setFillColor(sf::Color(0, 255, 255, 60));
            pulseSpeed = 2.0f;
            rotationSpeed = 150.f;
            break;
    }
}

void PowerUp::update(float deltaTime) {
    if (collected) return;
    
    shape.move(speed * deltaTime, 0);
    glowShape.move(speed * deltaTime, 0);
    
    // Animation de pulsation
    animationTime += deltaTime;
    float pulse = 0.8f + 0.2f * std::sin(animationTime * pulseSpeed * 3.14159f);
    shape.setScale(pulse, pulse);
    
    // Animation de rotation
    shape.rotate(rotationSpeed * deltaTime);
    
    // Animation de la lueur
    float glowPulse = 0.6f + 0.4f * std::sin(animationTime * pulseSpeed * 2.0f * 3.14159f);
    sf::Color glowColor = glowShape.getFillColor();
    glowColor.a = static_cast<sf::Uint8>(60 + 40 * glowPulse);
    glowShape.setFillColor(glowColor);
    glowShape.setScale(1.0f + 0.2f * glowPulse, 1.0f + 0.2f * glowPulse);
}

void PowerUp::draw(sf::RenderTarget& target) const {
    if (collected) return;
    
    target.draw(glowShape);
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