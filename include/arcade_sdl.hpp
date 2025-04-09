/*
** EPITECH PROJECT, 2025
** SDL Graphical Library
** File description:
** SDL implementation of IGraphical
*/



#ifndef ARCADE_SDL_HPP
#define ARCADE_SDL_HPP

#include "IGraphical.hpp"
#include <SDL2/SDL.h>
#include <SDL2/SDL_ttf.h>

class ArcadeSDL : public IGraphical {
public:
    ArcadeSDL();
    ~ArcadeSDL() override;

    void init() override;
    void close() override;
    void render(const RenderData &data) override;
    int getInput() override;
    std::string getPlayerName() override;
    std::string displayMenu(const std::vector<std::string>& games, 
                          const std::vector<std::string>& graphics,
                          const std::vector<std::pair<std::string, int>>& scores) override;

private:
    SDL_Window* window;
    SDL_Renderer* renderer;
    TTF_Font* font;
    void drawEntity(const GameEntity &entity);
    void drawText(const GameText &text);
};

extern "C" {
    IGraphical* createGraphical();
    void deleteGraphical(IGraphical* graphical);
}

#endif