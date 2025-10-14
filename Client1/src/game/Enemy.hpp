#ifndef ENEMY_HPP
#define ENEMY_HPP

#include <SFML/Graphics.hpp>

class Enemy {
private:
    sf::RectangleShape shape;    // Ancien système (inutilisé)
    sf::CircleShape circle;      // Fallback
    sf::Sprite sprite;
    sf::Texture texture;
    float speed;
    int hp;
    int maxHp;
    int type;
    int points;
    bool shoots;
    bool usingTexture = false;
    float animationTime;
    int currentFrame;
    int frameWidth;
    int frameHeight;
    
    // Pour l'effet de dégâts
    float damageFlashTime = 0.f;
    sf::Color originalColor;
    
public:
    Enemy(float x, float y, int level);
    void update(float deltaTime);
    void draw(sf::RenderTarget& target);
    void takeDamage(int damage);
    bool isDead() const;
    bool canShoot() const;
    int getPoints() const;
    sf::FloatRect getBounds() const;
    sf::Vector2f getPosition() const;
};

#endif