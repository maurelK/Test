#ifndef NIBBLER_HPP
#define NIBBLER_HPP

#include "IGame.hpp"
#include <vector>
#include <string>

#ifndef KEY_UP
#define KEY_UP 0403
#define KEY_DOWN 0402
#define KEY_LEFT 0404
#define KEY_RIGHT 0405
#endif

class Nibbler : public IGame
{
public:
    Nibbler();
    void init() override;
    void update() override;
    bool handleInput(int key) override;
    const RenderData &getRenderData() const override;
    std::string getName() const override { return "Nibbler"; }
    int getScore() const override { return score; }

private:
    enum Direction
    {
        UP,
        DOWN,
        LEFT,
        RIGHT
    };

    struct Wall
    {
        int x, y;
        int width, height;
    };

    std::vector<std::pair<int, int>> body;
    std::vector<std::pair<int, int>> foods;
    std::vector<Wall> walls;
    Direction currentDir;
    Direction nextDir;
    RenderData renderData;
    int score;
    bool gameOver;
    int gridWidth = 30;
    int gridHeight = 20;
    float moveTimer = 0;
    const float moveInterval = 0.15f;

    void generateMaze();
    void spawnFood(int count = 5);
    bool checkCollision() const;
    void moveSnake();
    void checkFood();
    void updateRenderData();
    bool isPositionFree(int x, int y) const;
    void handleWallCollision();
};

extern "C"
{
    IGame *createGame();
    void deleteGame(IGame *game);
}

#endif