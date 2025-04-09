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

std::string ArcadeNcurses::displayMenu(const std::vector<std::string>& games, 
        const std::vector<std::string>& graphics,
        const std::vector<std::pair<std::string, int>>& scores)
{
    WINDOW* gameWin = newwin(15, 40, 2, 2);
    WINDOW* graphicWin = newwin(15, 40, 2, 45);
    WINDOW* scoreWin = newwin(10, 80, 18, 2);
    WINDOW* nameWin = newwin(3, 40, 20, 2);

    // Afficher les jeux
    wclear(gameWin);
    box(gameWin, 0, 0);
    mvwprintw(gameWin, 1, 1, "Available Games:");
    for (size_t i = 0; i < games.size(); ++i) {
    mvwprintw(gameWin, 3 + i, 2, "%s", games[i].c_str());
    }

    // Afficher les libs graphiques
    wclear(graphicWin);
    box(graphicWin, 0, 0);
    mvwprintw(graphicWin, 1, 1, "Graphics Libraries:");
    for (size_t i = 0; i < graphics.size(); ++i) {
    mvwprintw(graphicWin, 3 + i, 2, "%s", graphics[i].c_str());
    }

    // Afficher les scores
    wclear(scoreWin);
    box(scoreWin, 0, 0);
    mvwprintw(scoreWin, 1, 1, "High Scores:");
    for (size_t i = 0; i < scores.size(); ++i) {
    mvwprintw(scoreWin, 3 + i, 2, "%s: %d", 
    scores[i].first.c_str(), scores[i].second);
    }

    // Gestion des inputs
    keypad(gameWin, TRUE);
    int highlight = 0;
    while (true) {
    // Mettre à jour la sélection
    for (size_t i = 0; i < games.size(); ++i) {
    mvwchgat(gameWin, 3 + i, 2, -1, 
    (i == highlight) ? A_REVERSE : A_NORMAL, 0, NULL);
    }
    wrefresh(gameWin);

    int ch = wgetch(gameWin);
    switch (ch) {
    case KEY_UP: highlight = (highlight - 1) % games.size(); break;
    case KEY_DOWN: highlight = (highlight + 1) % games.size(); break;
    case 10: return games[highlight];
    case 27: return "";
    }
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