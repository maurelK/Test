#include "tcp_server.hpp"
#include <iostream>
#include "protocol.hpp"

TCPServer::TCPServer(unsigned short port) 
    : m_port(port)
    , m_accepteur(m_contexteIO, tcp::endpoint(tcp::v4(), port))
{
    std::cout << "Serveur TCP créé sur le port " << port << std::endl;
}

void TCPServer::demarrer()
{
    std::cout << "Démarrage du serveur TCP..." << std::endl;
    demarrerEcoute();
    
    m_contexteIO.run();
}

void TCPServer::arreter()
{
    std::cout << "Arrêt du serveur TCP..." << std::endl;
    m_contexteIO.stop();
}

void TCPServer::demarrerEcoute()
{
    auto nouveauSocket = std::make_shared<tcp::socket>(m_contexteIO);
    
    m_accepteur.async_accept(*nouveauSocket,
        [this, nouveauSocket](boost::system::error_code erreur) {
            if (!erreur) {
                gererNouvelleConnexion(nouveauSocket);
            }
            demarrerEcoute();
        });
}

void TCPServer::gererNouvelleConnexion(std::shared_ptr<tcp::socket> socket)
{
    std::cout << "Nouveau client connecté!" << std::endl;
    m_clients.push_back(socket);
    demarrerLecture(socket);
}

void TCPServer::gererDeconnexion(std::shared_ptr<tcp::socket> socket)
{
    if (socket->is_open()) {
        socket->close();
    }

    m_clients.erase(
        std::remove_if(m_clients.begin(), m_clients.end(),
            [socket](const std::shared_ptr<tcp::socket>& client) {
                return client == socket;
            }),
        m_clients.end()
    );
    
    std::cout << "Client retiré. Clients connectés: " << m_clients.size() << std::endl;
}

void TCPServer::demarrerLecture(std::shared_ptr<tcp::socket> socket) {
    auto buffer = std::make_shared<std::vector<char>>(1024);
    
    socket->async_read_some(boost::asio::buffer(*buffer),
        [this, socket, buffer](boost::system::error_code erreur, size_t taille) {
            if (!erreur) {
                std::cout << "Message reçu (" << taille << " octets)" << std::endl;
                if (taille < sizeof(PacketHeader)) {
                    std::cout << "Message trop court!" << std::endl;
                    demarrerLecture(socket);
                    return;
                }
                PacketHeader* entete = reinterpret_cast<PacketHeader*>(buffer->data());
    
                switch (entete->type) {
                    case PacketType::LOGIN: {
                        LoginPacket* login = reinterpret_cast<LoginPacket*>(buffer->data());
                        std::cout << "Login reçu: " << login->username << std::endl;
                        break;
                    }
                    case PacketType::CHAT_MESSAGE: {
                        ChatMessagePacket* chat = reinterpret_cast<ChatMessagePacket*>(buffer->data());
                        std::cout << "Chat: " << chat->message << std::endl;
                        break;
                    }
                    case PacketType::JOIN_LOBBY: {
                        JoinLobbyPacket* lobby = reinterpret_cast<JoinLobbyPacket*>(buffer->data());
                        std::cout << "Rejoindre lobby: " << lobby->lobby_id << std::endl;
                        break;
                    }
                    default:
                        std::cout << "Type de paquet inconnu: " << static_cast<int>(entete->type) << std::endl;
                        break;
                }
                demarrerLecture(socket);
                
            } else {
                std::cout << "client déconnecté" << std::endl;
                gererDeconnexion(socket);
            }
        });
}
void TCPServer::envoyerATous(const std::vector<char>& message) {
    for (auto& client : m_clients) {
        if (client->is_open()) {
            boost::asio::write(*client, boost::asio::buffer(message));
        }
    }
}