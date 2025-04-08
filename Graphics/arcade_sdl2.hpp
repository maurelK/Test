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
    void draw(const std::vector<std::string>& display) override;
    void refresh() override;
    void close() override;
    int getInput() override;
    void clear() override;
    std::string getPlayerName() override;
    std::string displayMenu(const std::vector<std::string> &games)override;
private:
    SDL_Window* window;
    SDL_Renderer* renderer;
    bool running;
};

//extern "C" void deleteInstance(IGraphical* lib);

#endif