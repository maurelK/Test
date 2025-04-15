/*
** EPITECH PROJECT, 2025
** SDL Graphical Library
** File description:
** SDL implementation of IGraphical
*/

#ifndef ARCADE_SDL_HPP
    #define ARCADE_SDL_HPP

    #include <string>
    #include <vector>
    #include <SDL2/SDL.h>
    #include <SDL2/SDL_ttf.h>
    #include "IGraphical.hpp"

class ArcadeSDL : public IGraphical {
public:
    ArcadeSDL();
    ~ArcadeSDL();
    bool init() override;
    void close() override;
    void render(const RenderData &data) override;
    int getInput() override;
    std::string getPlayerName() override;

private:
    SDL_Window *window;
    SDL_Renderer *renderer;
    TTF_Font *font;

    void renderText(const std::string &text, int x, int y, SDL_Color color);
    SDL_Color getColorFromCode(int colorCode);
};

#endif
