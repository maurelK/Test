#ifndef ENEMY_HPP
#define ENEMY_HPP

#include <SFML/Graphics.hpp>
#include "TextureManager.hpp"

enum class EnemyType {
    Type1 = 1,
    Type2 = 2,
    Type3 = 3
};

class Enemy {
private:
    sf::Sprite sprite;
    sf::CircleShape fallback;
    EnemyType type;
    float speed;
    float animationTime;
    bool alive;
    bool useSprite;
    bool shoots;
    int points;

public:
    Enemy(float x, float y, int typeId = 1);

    void update(float deltaTime);
    void draw(sf::RenderTarget& target) const;
    void takeDamage(int dmg);
    bool isDead() const { return !alive; }
    bool canShoot() const { return shoots; }
    int getPoints() const { return points; }

    sf::Vector2f getPosition() const;
    sf::FloatRect getBounds() const;
};

#endif
