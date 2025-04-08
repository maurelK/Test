/*
** EPITECH PROJECT, 2025
** arcade_sdl2.hpp
** File description:
** Implement the libraryarcade_sdl2.so
*/

#ifndef SDL_GRAPHICAL_HPP
#define SDL_GRAPHICAL_HPP
#include "IGraphical.hpp"
#include <SDL2/SDL.h>

class SDLGraphical : public IGraphical {
public:
    SDLGraphical();
    ~SDLGraphical() override;

    void init() override;
    void draw() override;
    void refresh() override;
    void close() override;
    int getInput() override;
    //void clear() override;
    //void drawTile(int x, int y, char c, int color) override;
    //void drawText(int x, int y, const std::string &text, int color) override;
    //void initColorPairs() override;
    std::string getPlayerName() override;
    std::string displayMenu(const std::vector<std::string> &games)override;
private:
    SDL_Window* window;
    SDL_Renderer* renderer;
    bool running;
};

//extern "C" void deleteInstance(IGraphical* lib);

#endif