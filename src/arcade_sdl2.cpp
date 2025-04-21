#include "../include/arcade_sdl2.hpp"
#include <iostream>
#include <stdexcept>
#include <chrono>

ArcadeSDL::ArcadeSDL() : window(nullptr), renderer(nullptr), font(nullptr) {}

ArcadeSDL::~ArcadeSDL() {
}

bool ArcadeSDL::init() {
    if (SDL_Init(SDL_INIT_VIDEO) < 0) {
        throw std::runtime_error("SDL_Init Error: " + std::string(SDL_GetError()));
    }

    if (TTF_Init() == -1) {
        throw std::runtime_error("TTF_Init Error: " + std::string(TTF_GetError()));
    }

    window = SDL_CreateWindow("Arcade Game", 
                             SDL_WINDOWPOS_CENTERED, 
                             SDL_WINDOWPOS_CENTERED, 
                             1600, 800, 
                             SDL_WINDOW_SHOWN);
    if (!window) {
        throw std::runtime_error("SDL_CreateWindow Error: " + std::string(SDL_GetError()));
    }

    renderer = SDL_CreateRenderer(window, -1, SDL_RENDERER_ACCELERATED);
    if (!renderer) {
        throw std::runtime_error("SDL_CreateRenderer Error: " + std::string(SDL_GetError()));
    }

    font = TTF_OpenFont("assets/font.ttf", 10);
    if (!font) {
        std::cerr << "Warning: Could not load font, using default SDL font" << std::endl;
    }
    return true;
}

void ArcadeSDL::close() {
    if (font) {
        TTF_CloseFont(font);
        font = nullptr;
    }
    if (renderer) {
        SDL_DestroyRenderer(renderer);
        renderer = nullptr;
    }
    if (window) {
        SDL_DestroyWindow(window);
        window = nullptr;
    }
    TTF_Quit();
    SDL_Quit();
}
SDL_Color ArcadeSDL::getColorFromCode(int colorCode) {
    switch (colorCode) {
        case 1: return {255, 255, 255, 255}; 
        case 2: return {255, 0, 0, 255};     
        case 3: return {0, 255, 0, 255};     
        case 4: return {0, 0, 255, 255};     
        case 5: return {255, 255, 0, 255};   
        default: return {255, 255, 255, 255};
    }
}

void ArcadeSDL::renderText(const std::string &text, int x, int y, SDL_Color color) {
    SDL_Surface* surface = TTF_RenderText_Solid(font, text.c_str(), color);
    if (!surface) return;

    SDL_Texture* texture = SDL_CreateTextureFromSurface(renderer, surface);
    if (!texture) {
        SDL_FreeSurface(surface);
        return;
    }

    SDL_Rect rect = {x, y, surface->w, surface->h};
    SDL_RenderCopy(renderer, texture, nullptr, &rect);
    
    SDL_DestroyTexture(texture);
    SDL_FreeSurface(surface);
}

void ArcadeSDL::render(const RenderData &data) {
    if (!renderer) return;

    SDL_SetRenderDrawColor(renderer, 0, 0, 0, 255);
    SDL_RenderClear(renderer);
    for (const auto &entity : data.entities) {
        SDL_Rect rect = {entity.x * 10, entity.y * 10, 10, 10};
        SDL_Color color = getColorFromCode(entity.color);
        SDL_SetRenderDrawColor(renderer, color.r, color.g, color.b, color.a);
        SDL_RenderFillRect(renderer, &rect);
    }
    for (const auto &text : data.texts) {
        SDL_Color color = getColorFromCode(text.color);
        renderText(text.content, text.x * 10, text.y * 10, color);
    }
    SDL_RenderPresent(renderer);
    SDL_Delay(16);
}

int ArcadeSDL::getInput() {
    static Uint32 lastInputTime = 0;
    const Uint32 inputDelay = 100;
    
    SDL_Event e;
    while (SDL_PollEvent(&e)) {
        if (e.type == SDL_QUIT) {
            close();
            SDL_DestroyWindow(window);
            return 27;
        }
        Uint32 currentTime = SDL_GetTicks();
        if (currentTime - lastInputTime < inputDelay) {
            continue;
        }
        if (e.type == SDL_KEYDOWN) {
            lastInputTime = currentTime;
            switch (e.key.keysym.sym) {
                case SDLK_UP: return 0;
                case SDLK_DOWN: return 1;
                case SDLK_LEFT: return 2;
                case SDLK_RIGHT: return 3;
                case SDLK_RETURN: return 10;
                case SDLK_n: return 'n';
                case SDLK_ESCAPE: return 27;
                default: return e.key.keysym.sym;
            }
        }
    }
    return -1;
}

std::string ArcadeSDL::getPlayerName() {
    return "Player";
}

extern "C" {
    IGraphical *createGraphical() {
        return new ArcadeSDL();
    }

    void deleteGraphical(IGraphical *graphical) {
        delete static_cast<ArcadeSDL *>(graphical);
    }
}