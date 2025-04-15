#include "../include/arcade_sdl.hpp"
#include <iostream>
#include <stdexcept>
#include <chrono>

ArcadeSDL::ArcadeSDL() : window(nullptr), renderer(nullptr), font(nullptr) {}

ArcadeSDL::~ArcadeSDL() {
    close();
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
        case 1: return {255, 255, 255, 255}; // White
        case 2: return {255, 0, 0, 255};     // Red
        case 3: return {0, 255, 0, 255};     // Green
        case 4: return {0, 0, 255, 255};     // Blue
        case 5: return {255, 255, 0, 255};   // Yellow
        default: return {255, 255, 255, 255};// Default white
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

    // Clear screen
    SDL_SetRenderDrawColor(renderer, 0, 0, 0, 255);
    SDL_RenderClear(renderer);

    // Render entities
    for (const auto &entity : data.entities) {
        SDL_Rect rect = {entity.x * 10, entity.y * 10, 10, 10};
        SDL_Color color = getColorFromCode(entity.color);
        SDL_SetRenderDrawColor(renderer, color.r, color.g, color.b, color.a);
        SDL_RenderFillRect(renderer, &rect);
    }

    // Render texts
    for (const auto &text : data.texts) {
        SDL_Color color = getColorFromCode(text.color);
        renderText(text.content, text.x * 10, text.y * 10, color);
    }

    SDL_RenderPresent(renderer);
}

int ArcadeSDL::getInput() {
    static Uint32 lastInputTime = 0;
    const Uint32 inputDelay = 200; // 200ms debounce delay
    
    SDL_Event e;
    while (SDL_PollEvent(&e)) {
        if (e.type == SDL_QUIT) {
            close();
            SDL_DestroyWindow(window);
            return 27; // ESC
        }

        Uint32 currentTime = SDL_GetTicks();
        if (currentTime - lastInputTime < inputDelay) {
            continue; // Skip input during debounce period
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
    return -1;  // No input detected
}

std::string ArcadeSDL::getPlayerName() {
    // Simple implementation - can be enhanced with SDL text input
    return "Player";
}

std::string ArcadeSDL::displayMenu(const std::vector<std::string> &games,
                                  const std::vector<std::string> &graphics,
                                  const std::vector<std::pair<std::string, int>> &scores) {
    /*int selected = 0;
    bool menuRunning = true;
    const int frameDelay = 1000/60; // 60 FPS
    
    while (menuRunning) {
        Uint32 frameStart = SDL_GetTicks();
        
        // Clear screen
        SDL_SetRenderDrawColor(renderer, 0, 0, 0, 255);
        SDL_RenderClear(renderer);

        // Render menu title
        renderText("ARCADE GAMES", 300, 50, {255, 255, 255, 255});

        // Render game options
        for (size_t i = 0; i < games.size(); ++i) {
            SDL_Color color = (i == selected) ? 
                SDL_Color{255, 255, 0, 255} : // Yellow for selected
                SDL_Color{255, 255, 255, 255}; // White for others
            renderText(games[i], 300, 100 + (i * 40), color);
        }

        // Render controls hint
        renderText("UP/DOWN: Select  ENTER: Choose  ESC: Exit", 200, 500, {200, 200, 200, 255});

        SDL_RenderPresent(renderer);

        // Handle input
        SDL_Event e;
        while (SDL_PollEvent(&e)) {
            if (e.type == SDL_QUIT) {
                close();
                return "";
            }
            if (e.type == SDL_KEYDOWN) {
                switch (e.key.keysym.sym) {
                    case SDLK_UP: 
                        selected = (selected - 1 + games.size()) % games.size();
                        break;
                    case SDLK_DOWN:
                        selected = (selected + 1) % games.size();
                        break;
                    case SDLK_RETURN:
                        menuRunning = false;
                        break;
                    case SDLK_ESCAPE:
                        return "";
                }
            }
        }

        // Frame rate control
        limitFramerate(frameStart);
    }
    return games[selected];*/
    return "";
}

//void ArcadeSDL::limitFramerate(Uint32 frameStart) {
//    const int frameDelay = 1000/60; // 60 FPS
//    int frameTime = SDL_GetTicks() - frameStart;
//    if (frameTime < frameDelay) {
//        SDL_Delay(frameDelay - frameTime);
//    }
//}

extern "C" {
    IGraphical *createGraphical() {
        return new ArcadeSDL();
    }

    void deleteGraphical(IGraphical *graphical) {
        delete static_cast<ArcadeSDL *>(graphical);
    }
}