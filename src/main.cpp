#include "../include/Acore.hpp"
#include <iostream>


#include "../include/Acore.hpp"
#include <iostream>

int main(int argc, char **argv)
{
    if (argc != 2) {
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