/*
** EPITECH PROJECT, 2025
** IGAME
** File description:
** IGAMEe of my program
*/

#ifndef IGAME_HPP
#define IGAME_HPP

#include <vector>
#include <string>

class IGame {
public:
    virtual ~IGame() = default;
    
    virtual void init() = 0;
    virtual void update() = 0;
    virtual void handleInput(int input) = 0;
    virtual const std::vector<std::string>& getDisplay() const = 0;
    virtual int getScore() const = 0;
    virtual bool isGameOver() const = 0;
};

extern "C" {
    IGame* createGameInstance();
    void deleteInstance(IGame* game);
}

#endif