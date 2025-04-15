/*
** EPITECH PROJECT, 2025
** Class Igraphical
** File description:
** IGraphical of my program
*/

#ifndef IGRAPHICAL_HPP
    #define IGRAPHICAL_HPP
    #include <string>
    #include <iostream>
    #include <string>
    #include <vector>
    #include "IGame.hpp"

class IGraphical
{
public:
    struct GameEntity {
        int x;
        int y;
        int width;
        int height;
        int color;
        char symbol;
        std::string spritePath; // Path to sprite image
        bool useSprite;         // Whether to use sprite instead of primitive
    };

    struct GameText {
        int x;
        int y;
        std::string content;
        int color;
        
        GameText(int x, int y, std::string content, int color)
            : x(x), y(y), content(std::move(content)), color(color) {}
    };

    struct RenderData {
        std::vector<GameEntity> entities;
        std::vector<GameText> texts;
    };

    virtual ~IGraphical() = default;
    virtual bool init() = 0;
    virtual void close() = 0;
    virtual void render(const RenderData &data) = 0;
    virtual int getInput() = 0;
    virtual std::string getPlayerName() = 0;
    virtual std::string displayMenu(const std::vector<std::string>& games, 
        const std::vector<std::string>& graphics,
        const std::vector<std::pair<std::string, int>>& scores) = 0;

};

extern "C"
{
    IGraphical *createGraphical();
    void deleteGraphical(IGraphical *graphical);
}

#endif