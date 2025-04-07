/*
** EPITECH PROJECT, 2025
** Ncurses Igraphical
** File description:
** Graphical library ncurses of my program
*/

#ifndef AR_NCURSES
#define AR_NCURSES

    #include <string>
    #include <iostream>
    #include "IGraphical.hpp"
    #include <ncurses.h>
class arcade_ncuses : public IGraphical {
public:
    arcade_ncuses();
    ~arcade_ncuses();
    void init() override;
    void draw() override;
    void refresh();
    void close();
    int getInput();
    std::string getPlayerName() override;
    std::string displayMenu(const std::vector<std::string> &games)override;
};

#endif