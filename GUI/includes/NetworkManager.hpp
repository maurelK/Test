#ifndef NETWORKMANAGER_HPP
    #define NETWORKMANAGER_HPP
    #include <SFML/Network.hpp>
    #include <string>

class NetworkManager {
public:
    NetworkManager();
    void connectToServer(const std::string &ip, unsigned short port);
    void sendMessage(const std::string &message);
    std::string receiveMessage();

private:
    sf::TcpSocket socket;
};

#endif
