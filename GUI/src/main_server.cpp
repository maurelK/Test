#include "Server.hpp"

int main(int ac, char **av)
{
    Server server(std::atoi(av[1]));
    server.run();
    return 0;
}
