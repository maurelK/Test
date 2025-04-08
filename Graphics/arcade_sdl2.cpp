/*
** EPITECH PROJECT, 2025
** SDL2 Graphics Implementation 
** File description:
** Complete SDL2 graphics library for Arcade
*/
#include "arcade_sdl2.hpp"
#include <SDL2/SDL_ttf.h>
#include <iostream>
#include <vector>
#include <string>
SDLGraphical::SDLGraphical() : window(nullptr), renderer(nullptr), running(true) 
{
    if (TTF_Init() == -1) {
        std::cerr << "TTF_Init failed: " << TTF_GetError() << std::endl;
    }
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

void SDLGraphical::draw(const std::vector<std::string>& display) 
{
    SDL_SetRenderDrawColor(renderer, 0, 0, 0, 255);
    SDL_RenderClear(renderer);

    // Calculate cell size based on display dimensions
    int cellSize = 20;
    int startX = (800 - (display[0].size() * cellSize)) / 2;
    int startY = (600 - (display.size() * cellSize)) / 2;

    for (size_t y = 0; y < display.size(); y++) {
        for (size_t x = 0; x < display[y].size(); x++) {
            char c = display[y][x];
            SDL_Rect rect = {
                static_cast<int>(startX + (x * cellSize)),
                static_cast<int>(startY + (y * cellSize)),
                cellSize,
                cellSize
            };

            // Set color based on character
            switch (c) {
                case '#': // Walls
                    SDL_SetRenderDrawColor(renderer, 0, 0, 255, 255);
                    break;
                case 'O': // Player
                    SDL_SetRenderDrawColor(renderer, 0, 255, 0, 255);
                    break;
                case 'X': // Enemies/obstacles
                    SDL_SetRenderDrawColor(renderer, 255, 0, 0, 255);
                    break;
                case '.': // Collectibles
                    SDL_SetRenderDrawColor(renderer, 255, 255, 0, 255);
                    break;
                default: // Background
                    SDL_SetRenderDrawColor(renderer, 50, 50, 50, 255);
            }

            SDL_RenderFillRect(renderer, &rect);
        }
    }
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

void SDLGraphical::clear() 
{
    SDL_SetRenderDrawColor(renderer, 0, 0, 0, 255);
    SDL_RenderClear(renderer);
}
std::string SDLGraphical::getPlayerName() 
{
    // Implémentation basique - à améliorer avec une interface SDL
    return "Player1";
}

std::string SDLGraphical::displayMenu(const std::vector<std::string>& games)
{
    // Implémentation basique - à améliorer
    return games.empty() ? "" : games[0];
}

extern "C" {
    __attribute__((visibility("default"))) IGraphical* createGraphicalInstance() 
    {
        return new SDLGraphical();
    }

    __attribute__((visibility("default"))) void deleteInstance(IGraphical* lib) 
    {
        delete lib;
    }
}
