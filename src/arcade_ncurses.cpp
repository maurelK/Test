#include "../include/arcade_ncurses.hpp"
#include "arcade_ncurses.hpp"

ArcadeNcurses::ArcadeNcurses()
{
    initColors();
}

ArcadeNcurses::~ArcadeNcurses()
{
    close();
}

void ArcadeNcurses::close()
{
    endwin();
}
void ArcadeNcurses::init()
{
    initscr();
    cbreak();
    noecho(); 
    keypad(stdscr, TRUE); 
    curs_set(0);
    timeout(100);
}

void ArcadeNcurses::initColors()
{
    start_color();
    init_pair(1, COLOR_RED, COLOR_BLACK);
    init_pair(2, COLOR_GREEN, COLOR_BLACK);
    init_pair(3, COLOR_YELLOW, COLOR_BLACK);
    init_pair(4, COLOR_CYAN, COLOR_BLACK);
    init_pair(5, COLOR_MAGENTA, COLOR_BLACK);
}

void ArcadeNcurses::render(const RenderData &data)
{
    clear();
    for (const auto &entity : data.entities) {
        attron(COLOR_PAIR(entity.color));
        mvaddch(entity.y, entity.x, entity.symbol);
        attroff(COLOR_PAIR(entity.color));
    }

    for (const auto &text : data.texts) {
        attron(COLOR_PAIR(4));
        mvprintw(text.y, text.x, "%s", text.content.c_str());
        attroff(COLOR_PAIR(4));
    }
    refresh();
}

int ArcadeNcurses::getInput()
{
    return getch();
}

std::string ArcadeNcurses::getPlayerName()
{
    echo();
    char name[128];
    mvprintw(0, 0, "Enter your name: ");
    getnstr(name, sizeof(name) - 1);
    noecho();
    return std::string(name);
}

std::string ArcadeNcurses::displayMenu(const std::vector<std::string> &gameLibs) {
    int highlight = 0;
    int input = 0;

    while (true) {
        clear();
        mvprintw(0, 0, "== Select a Game ==");
        for (size_t i = 0; i < gameLibs.size(); ++i) {
            if ((int)i == highlight)
                attron(A_REVERSE);
            mvprintw(2 + i, 2, gameLibs[i].c_str());
            if ((int)i == highlight)
                attroff(A_REVERSE);
        }
        refresh();

        input = getch();
        if (input == KEY_UP)
            highlight = (highlight - 1 + gameLibs.size()) % gameLibs.size();
        else if (input == KEY_DOWN)
            highlight = (highlight + 1) % gameLibs.size();
        else if (input == '\n')
            return gameLibs[highlight];
    }
}


extern "C"
{
    IGraphical *createGraphical()
    {
        return new ArcadeNcurses();
    }

    void deleteGraphical(IGraphical *graphical)
    {
        delete static_cast<ArcadeNcurses *>(graphical);
    }
}
