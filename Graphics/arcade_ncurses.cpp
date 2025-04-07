#include "arcade_ncurses.hpp"

arcade_ncuses::arcade_ncuses()
{}

arcade_ncuses::~arcade_ncuses()
{
    endwin();
}

void arcade_ncuses::init()
{
    initscr();
}

void arcade_ncuses::draw()
{
    clear();
    mvprintw(200, 500, "Arcade - Ncurses");
}

void arcade_ncuses::refresh()
{
    ::refresh;
}

void arcade_ncuses::close()
{
    endwin();
}

int arcade_ncuses::getInput()
{
    return(getch());
}


std::string arcade_ncuses::displayMenu(const std::vector<std::string> &games)
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
    return new arcade_ncuses();
}