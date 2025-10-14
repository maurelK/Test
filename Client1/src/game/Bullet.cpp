#include "Bullet.hpp"
#include <cmath>
#include <iostream>

Bullet::Bullet(float x, float y, bool fromPlayer)
    : speed(fromPlayer ? 700.f : -500.f),
      fromPlayer(fromPlayer),
      animationTime(0.f)
{
    try {
        sprite.setTexture(TextureManager::get("assets/bullet.png"));
        sprite.setScale(0.4f, 0.4f);
        sprite.setPosition(x, y);
        useSprite = true;
    } catch (...) {
        useSprite = false;
    }


    float radius = fromPlayer ? 8.f : 6.f;
    circle.setRadius(radius);
    circle.setOrigin(radius, radius);
    circle.setPosition(x, y);

    if (fromPlayer) {
        circle.setFillColor(sf::Color(255, 200, 0, 230));
        circle.setOutlineColor(sf::Color(255, 255, 100));
    } else {
        circle.setFillColor(sf::Color(255, 50, 50, 230));
        circle.setOutlineColor(sf::Color(255, 100, 100));
    }
    circle.setOutlineThickness(2.f);
}

void Bullet::update(float deltaTime) {
    float dx = speed * deltaTime;
    if (useSprite)
        sprite.move(dx, 0);
    circle.move(dx, 0);

    animationTime += deltaTime * 10.f;
    float pulse = 1.f + 0.2f * std::sin(animationTime);
    circle.setScale(pulse, pulse);
    circle.rotate(200.f * deltaTime);
}

void Bullet::draw(sf::RenderTarget& target) {
    if (useSprite) {
        target.draw(sprite);
    } else {
        target.draw(circle);

        sf::CircleShape glow = circle;
        glow.setRadius(circle.getRadius() * 0.5f);
        glow.setOrigin(glow.getRadius(), glow.getRadius());
        glow.setPosition(circle.getPosition());
        glow.setFillColor(sf::Color(255, 255, 255, 200));
        target.draw(glow);
    }
}

bool Bullet::isOutOfBounds(float screenWidth) const {
    float x = useSprite ? sprite.getPosition().x : circle.getPosition().x;
    return x > screenWidth + 50 || x < -50;
}

sf::FloatRect Bullet::getBounds() const {
    return useSprite ? sprite.getGlobalBounds() : circle.getGlobalBounds();
}
