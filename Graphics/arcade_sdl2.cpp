/*
** EPITECH PROJECT, 2025
** Cpp
** File description:
** Hpp
*/
#include "arcade_sdl2.hpp"
SDLGraphical::SDLGraphical() : window(nullptr), renderer(nullptr), running(true) 
{
}

SDLGraphical::~SDLGraphical() 
{
    close();
}

void SDLGraphical::init() 
{
    if (SDL_Init(SDL_INIT_VIDEO) < 0) {
        return;
    }
    window = SDL_CreateWindow("Arcade SDL",
        SDL_WINDOWPOS_CENTERED,
        SDL_WINDOWPOS_CENTERED,
        800, 600,
        SDL_WINDOW_SHOWN);
    if (!window) {
        std::cerr << "Window creation a échoué petit Tundé " << SDL_GetError() << std::endl;
        return;
        }
    renderer = SDL_CreateRenderer(window, -1, SDL_RENDERER_ACCELERATED);
    if (!renderer) {
        return;
    }
}

void SDLGraphical::draw() 
{
    SDL_SetRenderDrawColor(renderer, 0, 0, 0, 255);
    SDL_RenderClear(renderer);
    SDL_SetRenderDrawColor(renderer, 0, 255, 0, 255);
    SDL_Rect rect = {100, 100, 50, 50};
    SDL_RenderFillRect(renderer, &rect);
}

void SDLGraphical::refresh() 
{
    SDL_RenderPresent(renderer);
}

void SDLGraphical::close() 
{
    if (renderer) {
        SDL_DestroyRenderer(renderer);
        renderer = nullptr;
    }
    if (window) {
        SDL_DestroyWindow(window);
        window = nullptr; 
    }
    SDL_Quit();
}

int SDLGraphical::getInput() 
{
    SDL_Event event;
    while (SDL_PollEvent(&event)) {
        if (event.type == SDL_QUIT)
            running = false;
        if (event.type == SDL_KEYDOWN)
            return event.key.keysym.sym;
    }
    return 0;
}

//void SDLGraphical::clear() 
//{
//    SDL_RenderClear(renderer);
//}
//
//void SDLGraphical::drawTile(int x, int y, char c, int color) 
//{
//    SDL_Rect rect = { x * 20, y * 20, 20, 20 };
//    SDL_SetRenderDrawColor(renderer, 255, 255, 255, 255);
//    SDL_RenderFillRect(renderer, &rect);
//}
//
//void SDLGraphical::drawText(int x, int y, const std::string &text, int color) 
//{
//    std::cout << "drawText comme " << x << "," << y << ": " << text << std::endl;
//}
//
//void SDLGraphical::initColorPairs() 
//{
//}

extern "C" IGraphical* createInstance() 
{
    return new SDLGraphical();
}

//extern "C" void deleteInstance(IGraphical* lib) 
//{
//    delete lib;
//}