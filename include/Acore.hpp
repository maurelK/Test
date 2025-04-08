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
    #include <ncurses.h>  // pour KEY_UP, KEY_DOWN


class Acore
{
public:
    Acore();
    ~Acore();

    void run(const std::string &graphicalLibPath, const std::string &gameLibPath);
    void runMenu(const std::string &defaultGraphicalLib);
private:
    bool isValidLibrary(const std::string &path, const std::string &symbol);
};

#endif