/*
** EPITECH PROJECT, 2025
** Arcade
** File description:
** Core header for Arcade project
*/

#ifndef ACORE_HPP_
#define ACORE_HPP_

#include <vector>
#include <string>
#include <map>
#include <memory>
#include <chrono>
#include <dirent.h>
#include <dlfcn.h>
#include <iostream>
#include <fstream>
#include <filesystem>
#include "../Graphics/IGraphical.hpp"
#include "../Games/IGame.hpp"
#include <algorithm>


class Acore {
public:
    struct MenuChoice {
        std::string selectedGame;
        std::string selectedGraphic;
        std::string playerName;
    };

    Acore() = default;
    ~Acore() = default;

    int libRead(const std::string &path);
    int RunMenu(std::string default_lib);
    void runGame(const MenuChoice& choice);

//private:
    std::vector<std::string> Graphics_lib;
    std::vector<std::string> Games_lib;
    std::map<std::string, std::vector<int>> scores;

    bool isGraphicalLib(const std::string &filename);
    bool isGameLib(const std::string &filename);
    
    template <typename T>
    std::pair<std::unique_ptr<T>, void*> libLoading(const std::string &lib, const std::string &symbol);

    //void switchGraphics(const std::string& newLib);
    //void saveScore(const std::string& game, const std::string& player, int score);
    //void loadScores();
};

#endif /* ACORE_HPP_ */