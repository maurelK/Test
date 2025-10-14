#ifndef GAME_HPP
#define GAME_HPP

#include <SFML/Graphics.hpp>
#include <vector>
#include <string>
#include "Player.hpp"
#include "Enemy.hpp"
#include "Bullet.hpp"
#include "PowerUp.hpp"

struct GameParticle {
    sf::Vector2f pos;
    sf::Vector2f velocity;
    sf::Color color;
    float life;
    float maxLife;
    
    GameParticle(float x, float y, sf::Color c) 
        : pos(x, y), 
          velocity((rand() % 200 - 100) / 10.f, (rand() % 200 - 100) / 10.f),
          color(c), life(30.f), maxLife(30.f) {}
    
    void update(float dt) {
        pos += velocity * dt * 60.f;
        life -= dt * 60.f;
        color.a = static_cast<sf::Uint8>((life / maxLife) * 255);
    }
    
    void draw(sf::RenderTarget& target) {
        sf::CircleShape shape(3);
        shape.setPosition(pos);
        shape.setFillColor(color);
        target.draw(shape);
    }
};

struct LevelTheme {
    sf::Color bg;
    sf::Color stars;
    std::string name;
};

enum class GameState {
    Playing, LevelTransition, GameOver
};

enum class GameModeType {
    Solo,
    Multiplayer
};

class Game {
private:
    sf::RenderWindow window;
    GameState state;
    GameModeType gameMode;
    
    Player player;
    std::vector<Enemy> enemies;
    std::vector<Bullet> bullets;
    std::vector<Bullet> enemyBullets;
    std::vector<PowerUp> powerUps;
    std::vector<GameParticle> particles;
    
    int score;
    int lives;
    int level;
    int enemiesKilled;
    
    sf::Clock clock;
    sf::Clock enemySpawnClock;
    sf::Clock transitionClock;
    sf::Font font;
    sf::RectangleShape playAgainButton;
    sf::Text playAgainText;
    std::vector<LevelTheme> themes;
    
    void handleEvents();
    void update(float deltaTime);
    void draw();
    
    void spawnEnemy();
    void checkCollisions();
    void levelUp();
    void gameOver();
    void createParticles(float x, float y, sf::Color color, int count = 10);

public:
    Game(GameModeType mode = GameModeType::Solo);
    void run();
    int getFinalScore() const { return score; }
    bool isVictory() const { return state != GameState::GameOver; }
};

#endif
