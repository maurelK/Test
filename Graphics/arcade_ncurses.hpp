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
class arcade_ncurses : public IGraphical {
public:
    arcade_ncurses();
    ~arcade_ncurses();
    void init() override;
    void clear() override;
    void draw(const std::vector<std::string>& display) override;
    void refresh() override;
    void close() override;
    int getInput() override;
    std::string getPlayerName() override;
    std::string displayMenu(const std::vector<std::string> &games)override;
};

#endif