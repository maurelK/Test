/*
** EPITECH PROJECT, 2025
** IGAME
** File description:
** IGAMEe of my program
*/

#ifndef IGAME_HPP
    #define IGAME_HPP
    #include <cstddef>
    #include <vector>
    #include <string>
    #include <unordered_map>

struct GameEntity
{
    int x, y;
    char symbol;
    int color;
};

struct GameText
{
    int x, y;
    std::string content;
    int color;
};

struct RenderData
{
    std::vector<GameEntity> entities;
    std::vector<GameText> texts; 
    bool shouldClose;
};

class IGame
{
public:
    virtual ~IGame() = default;

    virtual void init() = 0;      
    virtual void update() = 0; 
    virtual void handleInput(int key) = 0;
    virtual const RenderData &getRenderData() const = 0;
    virtual std::string getName() const { return "Unnamed Game"; }
    virtual int getScore() const { return 0; }
};

extern "C"
{
    IGame *createGame();
    void deleteGame(IGame *game);
}

#endif