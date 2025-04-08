/*
** EPITECH PROJECT, 2025
** IGAME
** File description:
** IGAMEe of my program
*/

#ifndef ARCADE_SNAKE_HPP
#define ARCADE_SNAKE_HPP

#include "IGame.hpp"
#include <vector>
#include <utility>
#include <random>


class SnakeGame : public IGame {
public:
    SnakeGame();
    ~SnakeGame() override = default;

    void init() override;
    void update() override;
    void handleInput(int input) override;
    
    GameState getState() const override { return _state; }
    size_t getScore() const override { return _score; }
    size_t getHighScore() const override { return _highScore; }
    float getElapsedTime() const override { return _elapsedTime; }
    float getFrameRate() const override { return _frameRate; }

private:
    enum Direction { UP, DOWN, LEFT, RIGHT };
    
    struct Position {
        int x;
        int y;
        bool operator==(const Position& other) const {
            return x == other.x && y == other.y;
        }
    };

    void spawnFood();
    bool checkCollision() const;
    void moveSnake();
    void growSnake();
    void resetGame();
    void updateHighScore();

    std::vector<Position> _snake;
    Position _food;
    Direction _currentDir;
    Direction _nextDir;
    GameState _state;
    size_t _score;
    size_t _highScore;
    float _elapsedTime;
    float _frameRate;
    float _moveInterval;
    float _timeSinceLastMove;
    bool _wallCollision;
    
    std::mt19937 _rng;
    std::uniform_int_distribution<int> _distX;
    std::uniform_int_distribution<int> _distY;
};

#endif