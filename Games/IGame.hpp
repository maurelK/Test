/*
** EPITECH PROJECT, 2025
** IGAME
** File description:
** IGAMEe of my program
*/

#ifndef IGAME_HPP
    #define IGAME_HPP
    #include <cstddef> 

class IGraphical;

enum GameState {
    RUNNING,
    PAUSED,
    GAME_OVER,
    MENU
};

class IGame {
public:
    virtual ~IGame() = default;
    virtual void init() = 0;
    virtual void update() = 0;
    virtual void handleInput(int input) = 0;
    virtual GameState getState() const = 0;
    virtual size_t getScore() const = 0;
    virtual size_t getHighScore() const = 0;
    virtual float getElapsedTime() const = 0;
    virtual float getFrameRate() const = 0;
};

extern "C" {
    IGame* createGameInstance();
    void deleteGameInstance(IGame* game);
}

#endif