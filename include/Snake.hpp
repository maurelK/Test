/*
** EPITECH PROJECT, 2025
** IGAME
** File description:
** IGAMEe of my program
*/

#ifndef SNAKE_HPP
#define SNAKE_HPP

#include "IGame.hpp"
#include <vector>
#ifndef KEY_UP
#define KEY_UP 0403
#define KEY_DOWN 0402
#define KEY_LEFT 0404
#define KEY_RIGHT 0405
#endif
class Snake : public IGame
{
public:
    Snake();
    void init() override;
    void update() override;
    bool handleInput(int key) override;
    const RenderData &getRenderData() const override;
    std::string getName() const override { return "Snake"; }
    int getScore() const override { return score; }

private:
    enum Direction
    {
        UP,
        DOWN,
        LEFT,
        RIGHT
    };

    std::vector<std::pair<int, int>> body; // Corps du serpent
    std::pair<int, int> food;              // Position de la nourriture
    Direction currentDir;
    Direction nextDir;
    RenderData renderData;
    int score;
    int highScore;
    bool gameOver;
    bool isPaused;
    float moveSpeed;
    float baseSpeed;
    int speedIncreaseThreshold;
    int gridWidth = 30;
    int gridHeight = 20;

    void spawnFood();
    bool checkCollision() const;
    void moveSnake();
    void growSnake();
    void updateRenderData(); // Met à jour renderData
};

extern "C"
{
    IGame *createGame();
    void deleteGame(IGame *game);
}

#endif