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
    int choice = 0;
    int input;

    keypad(stdscr, TRUE);
    curs_set(0);

    while (true) {
        clear();
        mvprintw(1, 2, "Select a game:");
        for (size_t i = 0; i < games.size(); ++i) {
            if ((int)i == highlight)
                attron(A_REVERSE);
            mvprintw(3 + i, 4, games[i].c_str());
            if ((int)i == highlight)
                attroff(A_REVERSE);
        }


        input = getch();
        switch (input) {
            case KEY_UP:
                highlight--;
                if (highlight < 0)
                    highlight = games.size() - 1;
                break;
            case KEY_DOWN:
                highlight++;
                if (highlight >= (int)games.size())
                    highlight = 0;
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