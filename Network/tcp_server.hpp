#pragma once

#include <boost/asio.hpp>
#include <vector>
#include <memory>

using boost::asio::ip::tcp;

class TCPServer {
public:
    TCPServer(unsigned short port);
    
    void demarrer();
    
    void arreter();

    void envoyerATous(const std::vector<char>& message);

private:
    void demarrerEcoute();
    
    void gererNouvelleConnexion(std::shared_ptr<tcp::socket> socket);

    void demarrerLecture(std::shared_ptr<tcp::socket> socket);
    
    void gererDeconnexion(std::shared_ptr<tcp::socket> socket);

    unsigned short m_port;
    boost::asio::io_context m_contexteIO;
    tcp::acceptor m_accepteur;
    std::vector<std::shared_ptr<tcp::socket>> m_clients;
};