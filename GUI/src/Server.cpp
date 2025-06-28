#include "Server.hpp"

Server::Server(unsigned short port)
{
    if (listener.listen(port) != sf::Socket::Done)
    {
        std::cerr << "Failed to start server on port " << port << std::endl;
    }
}

void Server::run()
{
    std::cout << "Server is listening on port 4242..." << std::endl;
    if (listener.accept(client) != sf::Socket::Done)
    {
        std::cerr << "Failed to accept client connection" << std::endl;
        return;
    }
    handleClient();
}

void Server::handleClient()
{
    char buffer[1024];
    std::size_t received;

    while (true)
    {
        if (client.receive(buffer, sizeof(buffer), received) != sf::Socket::Done)
        {
            std::cerr << "Error receiving data" << std::endl;
            break;
        }

        std::string command(buffer, received);
        std::cout << "Received: " << command << std::endl;

        if (command == "GRAPHIC\n")
        {
            std::string response = getServerInfo();
            client.send(response.c_str(), response.size() + 1);
        }
        else if (command == "msz\n")
        {
            std::string response = "msz 40 20\n";
            client.send(response.c_str(), response.size() + 1);
        }
        else
        {
            std::string response = "Unknown command\n";
            client.send(response.c_str(), response.size() + 1);
        }
    }
}

std::string Server::getServerInfo() const
{
    std::ostringstream oss;
    oss << "===================Zappy Server===================\n";
    oss << "port = 4242\n";
    oss << "width = 40\n";
    oss << "height = 20\n";
    oss << "clients_nb = 2\n";
    oss << "freq = 10\n";
    oss << "Teams [2]:\n";
    oss << "display eggs = [true]\n";
    oss << "name : [bot2]\n";
    oss << "nb_drones : [0]\n";
    oss << "nb_matures_eggs : [2]\n";
    oss << "name : [bot1]\n";
    oss << "nb_drones : [0]\n";
    oss << "nb_matures_eggs : [2]\n";
    oss << "verbose = 0\n";
    oss << "==================================================\n";
    return oss.str();
}