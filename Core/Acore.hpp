/*
** EPITECH PROJECT, 2025
** Core
** File description:
** Core of my program
*/
#ifndef CORE_HPP
    #define CORE_HPP
    #include <iostream>
    #include <string>
    #include <vector>
    #include <dirent.h>
    #include <dlfcn.h>
    #include <map>
    #include "../Graphics/IGraphical.hpp"
class Acore {
private:
    std::vector<std::string> Graphics_lib;
    std::vector<std::string> Games_lib;
    std::map<std::string, std::vector<int>> score;

public:
struct MenuChoice {
    std::string selectedGame;
    std::string selectedGraphic;
    std::string playerName;
};    
    int libRead(const std::string &path);
    bool Graphical_lib(const std::string &filename_comp);
    bool Game_lib(const std::string &filename_comp);
    template <typename T>
    T* libLoading(const std::string &lib, const std::string &symbol);
    int RunMenu(std::string default_lib);
};

#endif