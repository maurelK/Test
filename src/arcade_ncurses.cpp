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

void ArcadeNcurses::init() 
{
    initscr();
    /*if (LINES < 24 || COLS < 80) {
        endwin();
        throw std::runtime_error("Terminal too small! Minimum 80x24");
    }
    
    if (!has_colors()) {
        endwin();
        throw std::runtime_error("Terminal doesn't support colors");
    }*/

    cbreak();
    noecho();
    keypad(stdscr, TRUE);
    curs_set(0);
    timeout(100);
    start_color();
    initColors();
}

void ArcadeNcurses::close() 
{
    endwin();
}

void ArcadeNcurses::initColors()
{
    start_color();
    use_default_colors();
    init_pair(1, COLOR_RED, COLOR_BLACK);
    init_pair(2, COLOR_GREEN, COLOR_BLACK);
    init_pair(3, COLOR_YELLOW, COLOR_BLACK);
    init_pair(4, COLOR_CYAN, COLOR_BLACK);
    init_pair(5, COLOR_MAGENTA, COLOR_BLACK);
}

void ArcadeNcurses::render(const RenderData& data) 
{
    wclear(stdscr);
    
    // Render entities (game objects)
    for (const auto& entity : data.entities) {
        attron(COLOR_PAIR(entity.color));
        mvaddch(entity.y, entity.x, entity.symbol);
        attroff(COLOR_PAIR(entity.color));
    }
    
    // Render texts (UI elements)
    for (const auto& text : data.texts) {
        attron(COLOR_PAIR(text.color));
        mvprintw(text.y, text.x, "%s", text.content.c_str());
        attroff(COLOR_PAIR(text.color));
    }
    
    wnoutrefresh(stdscr);
    doupdate();
}


int ArcadeNcurses::getInput() {
    int ch = getch();
    switch (ch) {
        case KEY_UP:    return 0; // KEY_UP du core
        case KEY_DOWN:  return 1; // KEY_DOWN du core
        case KEY_LEFT:  return 2;
        case KEY_RIGHT: return 3;
        case 127:       // Backspace
        case KEY_BACKSPACE: return 4;
        case '\n':      return 10; // Touche Entrée
        default:        return ch;
    }
}

std::string ArcadeNcurses::getPlayerName() 
{
    echo();
    curs_set(1);
    
    char name[32];
    mvprintw(LINES/2, (COLS-30)/2, "Enter your name (max 30 chars): ");
    clrtoeol();
    move(LINES/2 + 1, (COLS-30)/2);
    
    int ch;
    int pos = 0;
    while ((ch = getch()) != '\n' && pos < 30) {
        if (ch == 127 || ch == KEY_BACKSPACE) {
            if (pos > 0) {
                pos--;
                addch('\b');
                clrtoeol();
            }
        }
        else if (isalnum(ch) || ch == ' ') {
            name[pos++] = ch;
            addch(ch);
        }
    }
    name[pos] = '\0';
    
    noecho();
    curs_set(0);
    return std::string(name);
}

std::string ArcadeNcurses::displayMenu(
    const std::vector<std::string>& games,
    const std::vector<std::string>& graphics,
    const std::vector<std::pair<std::string, int>>& scores)
{

    return ""; // Selection handled by core
}

// Factory functions
extern "C" IGraphical* createGraphical() 
{
    return new ArcadeNcurses();
}

extern "C" void deleteGraphical(IGraphical* graphical) 
{
    delete static_cast<ArcadeNcurses*>(graphical);
}