#include "Enemy.hpp"
#include <cstdlib>
#include <iostream>
#include <cmath>

#include "Enemy.hpp"
#include <cstdlib>
#include <iostream>

Enemy::Enemy(float x, float y, int level) : animationTime(0.f), currentFrame(0) {
    type = rand() % 4;
    
    // Stats basées sur le type
    switch(type) {
        case 0:
            speed = 150.f + level * 20.f;
            hp = maxHp = 1;
            points = 100;
            shoots = false;
            break;
        case 1:
            speed = 120.f + level * 15.f;
            hp = maxHp = 2;
            points = 200;
            shoots = true;
            break;
        case 2:
            speed = 100.f + level * 10.f;
            hp = maxHp = 3;
            points = 300;
            shoots = true;
            break;
        case 3:
            speed = 80.f + level * 10.f;
            hp = maxHp = 5;
            points = 500;
            shoots = true;
            break;
        default:
            speed = 150.f;
            hp = maxHp = 1;
            points = 100;
            shoots = false;
            break;
    }
    
    // TEMPORAIRE : Forcer les cercles pour tester
    std::string filename = "assets/enemy.png"; // Garder pour le message d'erreur
    usingTexture = false;
    /*
    // Chargement du sprite enemy.png
    usingTexture = texture.loadFromFile(filename);
    */
    
    if (usingTexture) {
        sprite.setTexture(texture);
        sf::Vector2u textureSize = texture.getSize();
        
        std::cout << "✓ Sprite enemy chargé : " << textureSize.x << "x" << textureSize.y << std::endl;
        
        // DIAGNOSTIC : Vérifier si l'image a une taille raisonnable
        if (textureSize.x == 0 || textureSize.y == 0) {
            std::cout << "PROBLÈME: L'image a une taille de 0 !" << std::endl;
            usingTexture = false;
        } else if (textureSize.x > 1000 || textureSize.y > 1000) {
            std::cout << "ATTENTION: L'image est très grande (" << textureSize.x << "x" << textureSize.y << ")" << std::endl;
        }
        
        if (usingTexture) {
            // Si votre PNG a plusieurs frames (animation), ajustez ici
            frameWidth = textureSize.x;
            frameHeight = textureSize.y;
            
            // OPTION : Utiliser seulement une partie de l'image (centre)
            // Si votre sprite est au centre de l'image 1024x1024, décommentez :
            /*
            int spriteSize = 256; // Taille du vrai sprite dans l'image
            int offsetX = (textureSize.x - spriteSize) / 2; // Centré
            int offsetY = (textureSize.y - spriteSize) / 2;
            sprite.setTextureRect(sf::IntRect(offsetX, offsetY, spriteSize, spriteSize));
            frameWidth = spriteSize;
            frameHeight = spriteSize;
            */
            
            // Pour l'instant on utilise toute l'image
            sprite.setTextureRect(sf::IntRect(0, 0, frameWidth, frameHeight));
            sprite.setPosition(x, y);
            
            // Taille finale à l'écran
            float targetSize = 30.f + type * 3.f; // Tailles différentes selon le type
            float scaleX = targetSize / frameWidth;
            float scaleY = targetSize / frameHeight;
            sprite.setScale(scaleX, scaleY);
            
            std::cout << "Sprite configuré - Taille finale: " << (frameWidth * scaleX) << "x" << (frameHeight * scaleY) << std::endl;
        }
    }
    
    if (!usingTexture) {
        // Fallback : cercles colorés si le sprite ne charge pas
        std::cerr << "✗ Impossible de charger " << filename << " -> Cercles de secours" << std::endl;
        
        circle.setRadius(15.f + type * 3.f);
        switch(type) {
            case 0: circle.setFillColor(sf::Color(255, 100, 100)); break;
            case 1: circle.setFillColor(sf::Color(255, 200, 0)); break;
            case 2: circle.setFillColor(sf::Color(200, 100, 255)); break;
            case 3: circle.setFillColor(sf::Color(0, 200, 255)); break;
        }
        circle.setOrigin(circle.getRadius(), circle.getRadius());
        circle.setPosition(x, y);
        
        frameWidth = circle.getRadius() * 2;
        frameHeight = circle.getRadius() * 2;
    }
}

void Enemy::update(float deltaTime) {
    sf::Vector2f movement(-speed * deltaTime, 0);
    
    // Toujours déplacer le sprite (même s'il n'est pas visible)
    sprite.move(movement);
}

void Enemy::draw(sf::RenderTarget& target) {
    if (usingTexture) {
        // DIAGNOSTIC : Dessiner un rectangle de fond pour voir où est le sprite
        sf::RectangleShape debugRect;
        debugRect.setSize(sf::Vector2f(frameWidth * sprite.getScale().x, frameHeight * sprite.getScale().y));
        debugRect.setPosition(sprite.getPosition());
        debugRect.setFillColor(sf::Color(255, 0, 0, 100)); // Rouge semi-transparent
        target.draw(debugRect);
        
        target.draw(sprite);
    }
    // Si pas de texture, on ne dessine rien (ennemis invisibles)
}

void Enemy::takeDamage(int damage) {
    hp -= damage;
    // Plus d'effet de flash - ennemis stables
}

bool Enemy::isDead() const {
    return hp <= 0;
}

bool Enemy::canShoot() const {
    return shoots;
}

int Enemy::getPoints() const {
    return points;
}

sf::FloatRect Enemy::getBounds() const {
    if (usingTexture) {
        return sprite.getGlobalBounds();
    } else {
        // Bounds fictifs pour les collisions même sans sprite visible
        sf::Vector2f pos = sprite.getPosition();
        return sf::FloatRect(pos.x, pos.y, frameWidth, frameHeight);
    }
}

sf::Vector2f Enemy::getPosition() const {
    return sprite.getPosition();
}