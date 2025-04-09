#include "../include/arcade_sdl.hpp"
#include <SDL2/SDL.h>
#include <SDL2/SDL_ttf.h>
#include <stdexcept>
#include <vector>
#include <string>

// Define keyboard constants if not available
#ifndef KEY_UP
#define KEY_UP 0
#define KEY_DOWN 1
#define KEY_LEFT 2
#define KEY_RIGHT 3
#endif

ArcadeSDL::ArcadeSDL() : window(nullptr), renderer(nullptr), font(nullptr) {}

ArcadeSDL::~ArcadeSDL()
{
    close();
}

void ArcadeSDL::init()
{
    if (SDL_Init(SDL_INIT_VIDEO) < 0) {
        throw std::runtime_error("SDL could not initialize: " + std::string(SDL_GetError()));
    }

    window = SDL_CreateWindow(
        "Arcade - SDL",
        SDL_WINDOWPOS_CENTERED,
        SDL_WINDOWPOS_CENTERED,
        800, 600,
        SDL_WINDOW_SHOWN
    );

    if (!window) {
        throw std::runtime_error("Window could not be created: " + std::string(SDL_GetError()));
    }

    renderer = SDL_CreateRenderer(window, -1, SDL_RENDERER_ACCELERATED);
    if (!renderer) {
        throw std::runtime_error("Renderer could not be created: " + std::string(SDL_GetError()));
    }

    if (TTF_Init() == -1) {
        throw std::runtime_error("SDL_ttf could not initialize: " + std::string(TTF_GetError()));
    }

    font = TTF_OpenFont("assets/font.ttf", 24);
    if (!font) {
        throw std::runtime_error("Failed to load font: " + std::string(TTF_GetError()));
    }

    SDL_SetRenderDrawColor(renderer, 0, 0, 0, 255);
    SDL_RenderClear(renderer);
    SDL_RenderPresent(renderer);
}

void ArcadeSDL::close()
{
    if (font) TTF_CloseFont(font);
    if (renderer) SDL_DestroyRenderer(renderer);
    if (window) SDL_DestroyWindow(window);
    TTF_Quit();
    SDL_Quit();
}

void ArcadeSDL::drawEntity(const GameEntity &entity)
{
    SDL_Rect rect = {
        entity.x * 20,  // Scale to screen size
        entity.y * 20,
        20, 20
    };

    // Set color based on entity color
    switch(entity.color) {
        case 1: SDL_SetRenderDrawColor(renderer, 255, 0, 0, 255); break;   // Red
        case 2: SDL_SetRenderDrawColor(renderer, 0, 255, 0, 255); break;   // Green
        case 3: SDL_SetRenderDrawColor(renderer, 255, 255, 0, 255); break; // Yellow
        case 4: SDL_SetRenderDrawColor(renderer, 0, 255, 255, 255); break; // Cyan
        default: SDL_SetRenderDrawColor(renderer, 255, 255, 255, 255);     // White
    }

    SDL_RenderFillRect(renderer, &rect);
}

void ArcadeSDL::drawText(const GameText &text)
{
    SDL_Color color = {255, 255, 255, 255}; // White
    SDL_Surface* surface = TTF_RenderText_Solid(font, text.content.c_str(), color);
    if (!surface) return;

    SDL_Texture* texture = SDL_CreateTextureFromSurface(renderer, surface);
    if (!texture) {
        SDL_FreeSurface(surface);
        return;
    }

    SDL_Rect rect = {text.x * 20, text.y * 20, surface->w, surface->h};
    SDL_RenderCopy(renderer, texture, NULL, &rect);

    SDL_DestroyTexture(texture);
    SDL_FreeSurface(surface);
}

void ArcadeSDL::render(const RenderData &data)
{
    SDL_SetRenderDrawColor(renderer, 0, 0, 0, 255);
    SDL_RenderClear(renderer);

    for (const auto &entity : data.entities) {
        drawEntity(entity);
    }

    for (const auto &text : data.texts) {
        drawText(text);
    }

    SDL_RenderPresent(renderer);
}

int ArcadeSDL::getInput()
{
    SDL_Event event;
    while (SDL_PollEvent(&event)) {
        switch (event.type) {
            case SDL_QUIT:
                return 27; // ESC
            case SDL_KEYDOWN:
                switch (event.key.keysym.sym) {
                    case SDLK_UP: return KEY_UP;
                    case SDLK_DOWN: return KEY_DOWN;
                    case SDLK_LEFT: return KEY_LEFT;
                    case SDLK_RIGHT: return KEY_RIGHT;
                    case SDLK_ESCAPE: return 27;
                    case SDLK_RETURN: return 10;
                    case SDLK_z: return 'z';
                    case SDLK_s: return 's';
                    case SDLK_q: return 'q';
                    case SDLK_d: return 'd';
                    case SDLK_r: return 'r';
                    case SDLK_n: return 'n';
                }
                break;
        }
    }
    return -1;
}

std::string ArcadeSDL::getPlayerName()
{
    std::string name;
    SDL_StartTextInput();
    SDL_Event event;
    bool done = false;

    while (!done && SDL_WaitEvent(&event)) {
        switch (event.type) {
            case SDL_QUIT:
                done = true;
                break;
            case SDL_KEYDOWN:
                if (event.key.keysym.sym == SDLK_RETURN) {
                    done = true;
                } else if (event.key.keysym.sym == SDLK_BACKSPACE && !name.empty()) {
                    name.pop_back();
                }
                break;
            case SDL_TEXTINPUT:
                name += event.text.text;
                break;
        }

        // Render input state
        SDL_SetRenderDrawColor(renderer, 0, 0, 0, 255);
        SDL_RenderClear(renderer);

        GameText prompt = {10, 10, "Enter your name: " + name, 1};
        drawText(prompt);

        SDL_RenderPresent(renderer);
    }
    SDL_StopTextInput();
    return name;
}

std::string ArcadeSDL::displayMenu(const std::vector<std::string>& games, 
                                 const std::vector<std::string>& graphics,
                                 const std::vector<std::pair<std::string, int>>& scores)
{
    int selected = 0;
    bool done = false;
    SDL_Event event;

    while (!done && SDL_WaitEvent(&event)) {
        switch (event.type) {
            case SDL_QUIT:
                return "";
            case SDL_KEYDOWN:
                switch (event.key.keysym.sym) {
                    case SDLK_UP:
                        selected = (selected - 1 + games.size()) % games.size();
                        break;
                    case SDLK_DOWN:
                        selected = (selected + 1) % games.size();
                        break;
                    case SDLK_RETURN:
                        return games[selected];
                    case SDLK_ESCAPE:
                        return "";
                }
                break;
        }

        // Render menu
        SDL_SetRenderDrawColor(renderer, 0, 0, 0, 255);
        SDL_RenderClear(renderer);

        // Draw title
        GameText title = {10, 5, "ARCADE MENU", 1};
        drawText(title);

        // Draw game list
        for (size_t i = 0; i < games.size(); ++i) {
            GameText item = {15, 10 + (int)i * 2, games[i], (i == selected) ? 2 : 3};
            drawText(item);
        }

        SDL_RenderPresent(renderer);
    }
    return "";
}

extern "C" {
    IGraphical* createGraphical()
    {
        return new ArcadeSDL();
    }

    void deleteGraphical(IGraphical* graphical)
    {
        delete static_cast<ArcadeSDL*>(graphical);
    }
}
