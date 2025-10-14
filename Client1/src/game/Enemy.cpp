#include "Enemy.hpp"
#include <cmath>
#include <iostream>

Enemy::Enemy(float x, float y, int typeId)
    : type(static_cast<EnemyType>(typeId)),
      speed(100.f + (typeId * 30.f)),
      animationTime(0.f),
      alive(true),
      useSprite(false)
{
    switch (type) {
        case EnemyType::Type1: shoots = false; points = 100; break;
        case EnemyType::Type2: shoots = true;  points = 200; break;
        case EnemyType::Type3: shoots = true;  points = 300; break;
    }

    try {
        std::string path;
        switch (type) {
            case EnemyType::Type1: path = "assets/enemy1.png"; break;
            case EnemyType::Type2: path = "assets/enemy2.png"; break;
            case EnemyType::Type3: path = "assets/enemy3.png"; break;
        }

        sprite.setTexture(TextureManager::get(path));
        sprite.setScale(0.2f, 0.2f);
        sprite.setPosition(x, y);
        useSprite = true;
        std::cout << "✓ Ennemi créé (Type " << typeId << ") - Sprite chargé\n";

    } catch (...) {
        useSprite = false;
        std::cout << "Ennemi Type " << typeId << " - Texture non trouvée, fallback cercle\n";
    }

    float radius = 25.f + typeId * 5.f;
    fallback.setRadius(radius);
    fallback.setOrigin(radius, radius);
    fallback.setPosition(x, y);

    sf::Color color;
    if (type == EnemyType::Type1) color = sf::Color(255, 100, 100);
    if (type == EnemyType::Type2) color = sf::Color(255, 180, 50);
    if (type == EnemyType::Type3) color = sf::Color(255, 255, 80);

    fallback.setFillColor(color);
    fallback.setOutlineColor(sf::Color::White);
    fallback.setOutlineThickness(2.f);
}

void Enemy::update(float deltaTime) {
    animationTime += deltaTime;

    float dy = std::sin(animationTime * 2.f) * 30.f;

    if (useSprite) {
        sprite.move(-speed * deltaTime, dy * deltaTime);
        sprite.rotate(10.f * deltaTime);
    } else {
        fallback.move(-speed * deltaTime, dy * deltaTime);
        fallback.rotate(10.f * deltaTime);
    }
}

void Enemy::draw(sf::RenderTarget& target) const {
    if (useSprite)
        target.draw(sprite);
    else
        target.draw(fallback);
}

void Enemy::takeDamage(int dmg) {
    alive = false;
}

sf::Vector2f Enemy::getPosition() const {
    return useSprite ? sprite.getPosition() : fallback.getPosition();
}

sf::FloatRect Enemy::getBounds() const {
    return useSprite ? sprite.getGlobalBounds() : fallback.getGlobalBounds();
}
