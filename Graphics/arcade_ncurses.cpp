#include "arcade_ncurses.hpp"



arcade_ncurses::arcade_ncurses() {
    initscr();
    cbreak();
    noecho();
    keypad(stdscr, TRUE);
    curs_set(0);
    timeout(100);
}

void arcade_ncurses::init() {
    // si nécessaire, mets le code que tu veux
}

arcade_ncurses::~arcade_ncurses() {
    endwin();
}

std::string arcade_ncurses::getPlayerName() {
    echo();
    char name[256];
    mvprintw(0, 0, "Enter your name: ");
    getnstr(name, sizeof(name)-1);
    noecho();
    return std::string(name);
}


void arcade_ncurses::draw()
{
    clear();
    mvprintw(200, 500, "Arcade - Ncurses");
}

void arcade_ncurses::refresh()
{
    ::refresh();
}

void arcade_ncurses::close()
{
    endwin();
}

int arcade_ncurses::getInput()
{
    return(getch());
}


std::string arcade_ncurses::displayMenu(const std::vector<std::string> &games)
{
    int highlight = 0;
    int input;
    int width, height;

    getmaxyx(stdscr, height, width);
    keypad(stdscr, TRUE);
    curs_set(0);

    while (true) {
        clear();
        box(stdscr, 0, 0);

        std::string title = "Arcade Menu (Ncurses)";
        mvprintw(1, (width - title.size()) / 2, "%s", title.c_str());

        std::string info = "Use ↑ ↓ to navigate, Enter to select";
        mvprintw(3, (width - info.size()) / 2, "%s", info.c_str());

        int startY = height / 2 - games.size() / 2;

        for (size_t i = 0; i < games.size(); ++i) {
            int posX = (width - games[i].size()) / 2;
            if ((int)i == highlight)
                attron(A_REVERSE);
            mvprintw(startY + i, posX, "%s", games[i].c_str());
            if ((int)i == highlight)
                attroff(A_REVERSE);
        }

        input = getch();
        switch (input) {
            case KEY_UP:
                highlight = (highlight - 1 + games.size()) % games.size();
                break;
            case KEY_DOWN:
                highlight = (highlight + 1) % games.size();
                break;
            case 10: // Enter key
                return games[highlight];
        }

        refresh();
    }
}



extern "C" IGraphical* createInstance() {
    return new arcade_ncurses();
}