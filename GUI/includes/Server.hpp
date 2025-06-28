#ifndef SERVER_HPP
#define SERVER_HPP

#include <SFML/Network.hpp>
#include <iostream>
#include <string>
#include <sstream>

class Server
{
public:
    Server(unsigned short port);
    void run();

private:
    sf::TcpListener listener;
    sf::TcpSocket client;
    void handleClient();
    std::string getServerInfo() const;
};

#endif