#ifndef TESTGAME_HPP
#define TESTGAME_HPP

#include "WindowManager.hpp"
#include "GameRenderer.hpp"
#include "game/PowerUp.hpp"
#include <vector>

class TestGame {
private:
    WindowManager windowManager;
    GameRenderer renderer;
    std::vector<PowerUp> powerUps;
    sf::Clock clock;
    sf::Clock spawnClock;

    void handleEvents();
    void update(float deltaTime);
    void render();
    void spawnPowerUp();

public:
    TestGame();
    void run();
};

#endif // TESTGAME_HPP
