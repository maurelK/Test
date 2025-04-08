#include "../include/Acore.hpp"
#include <iostream>

int main(int argc, char **argv)
{
    if (argc != 3) {
        std::cerr << "Usage: ./arcade <graphical_library.so> <game_library.so>" << std::endl;
        std::cerr << "Example: ./arcade ./lib/arcade_ncurses.so ./lib/arcade_snake.so" << std::endl;
        return 84;
    }
    try {
        Acore core;
        core.runMenu(argv[1]);
    }
    catch (const std::exception &e) {
        std::cerr << "Error: " << e.what() << std::endl;
        return 84;
    }
    return 0;
}