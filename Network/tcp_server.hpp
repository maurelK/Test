#pragma once

#include <asio.hpp>
#include <vector>
#include <memory>
#include <map>
#include "LobbyManager.hpp"

using asio::ip::tcp;

class TCPServer {
public:
    TCPServer(unsigned short port);
    
    void demarrer();
    
    void arreter();

    void envoyerATous(const std::vector<char>& message);

    void envoyerAClient(std::shared_ptr<tcp::socket> socket, const std::vector<char>& message);

private:
    void demarrerEcoute();
    
    void gererNouvelleConnexion(std::shared_ptr<tcp::socket> socket);

    void demarrerLecture(std::shared_ptr<tcp::socket> socket);
    
    void gererDeconnexion(std::shared_ptr<tcp::socket> socket);

    unsigned short m_port;
    asio::io_context m_contexteIO;
    tcp::acceptor m_accepteur;
    std::vector<std::shared_ptr<tcp::socket>> m_clients;
    std::map<std::shared_ptr<tcp::socket>, uint32_t> m_socketToPlayerId;
    std::map<uint32_t, std::vector<std::shared_ptr<tcp::socket>>> m_lobbyToSockets;
    LobbyManager m_lobbyManager;
};
