#include "GUI.hpp"
#include <iostream>

int main(int ac, char **av)
{
    if (ac != 5) {
        std::cerr << "Usage: " << av[0] << " -p PORT -h HOST" << std::endl;
        return 1;
    }

    int port = -1;
    std::string host;

    for (int i = 1; i < ac; ++i) {
        std::string arg = av[i];
        if (arg == "-p" && i + 1 < ac) {
            port = std::stoi(av[++i]);
        } else if (arg == "-h" && i + 1 < ac) {
            host = av[++i];
        }
    }

    if (port == -1 || host.empty())  {
        std::cerr << "Invalid arguments. Please provide both port and host." << std::endl;
        return 1;
    }

    GUI gui(port, host);
    gui.run();
    
    return 0;
}
