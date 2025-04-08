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

class ArcadeNcurses : public IGraphical {
public:
    ArcadeNcurses();
    ~ArcadeNcurses() override;

    void init() override;
    void close() override;
    void render(const RenderData& data) override;
    int getInput() override;
    std::string getPlayerName() override;
    //std::string displayMenu(const std::vector<std::string>& games, const std::vector<std::string>& libs) override;
    std::string displayMenu(const std::vector<std::string>& games) override;

private:
    void initColors();
};

extern "C" {
    IGraphical* createGraphical();
    void deleteGraphical(IGraphical* graphical);
}

#endif