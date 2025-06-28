#include "NetworkManager.hpp"

NetworkManager::NetworkManager() {}

void NetworkManager::connectToServer(const std::string &ip, unsigned short port)
{
    sf::Socket::Status status = socket.connect(ip, port);
    if (status != sf::Socket::Done)
    {
        exit(84);
    }
}

void NetworkManager::sendMessage(const std::string &message)
{
    socket.send(message.c_str(), message.size() + 1);
}

std::string NetworkManager::receiveMessage()
{
    char data[1024];
    std::size_t received;
    socket.receive(data, sizeof(data), received);
    return std::string(data, received);
}
