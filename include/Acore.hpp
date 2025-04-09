/*
** EPITECH PROJECT, 2025
** Core
** File description:
** Core of my program
*/

#ifndef ACORE_HPP
#define ACORE_HPP

#include <string>
#include <vector>
#include <dlfcn.h>
#include <iostream>
#include <dirent.h>
#include <filesystem>
#include <algorithm>
#include <memory>
#include <fstream>
#include "IGraphical.hpp"
#include "IGame.hpp"
#define KEY_UP 0
#define KEY_DOWN 1
#define KEY_LEFT 2
#define KEY_RIGHT 3
#define KEY_BACKSPACE 4
class Acore {
public:
    struct MenuState {
        size_t selectedGame;
        size_t selectedGraphic;
        std::vector<std::string> gameLibs;
        std::vector<std::string> graphicLibs;
        std::vector<std::pair<std::string, int>> scores;
        std::string playerName;
    };


//public:

    Acore();
    ~Acore();
    bool isValidLibrary(const std::string &path, const std::string &symbol);
    void loadAvailableLibs();
    void switchGraphicalLib(const std::string& newLib, void*& handle, IGraphical*& graphical);
    void handleGlobalInput(int input, void*& handle, IGraphical*& graphical);
    void saveScore(const std::string& game, int score);
    void loadScores();
    void handleNameInput(int input);
    //void handleMenuNavigation(int input);
    void updateMenuRender(IGraphical::RenderData& renderData);
    void runMenu(const std::string& initialLib);
private:
    MenuState state;
    void* currentGameHandle = nullptr;
    IGame* currentGame = nullptr;

    void loadGame(const std::string& path);
    void unloadGame();
};

#endif