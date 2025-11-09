#include "tcp_server.hpp"
#include <iostream>
#include "protocol.hpp"
#include <cstring>

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

    m_socketToPlayerId.erase(socket);

    for (auto& pair : m_lobbyToSockets) {
        pair.second.erase(std::remove(pair.second.begin(), pair.second.end(), socket), pair.second.end());
    }

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
                        uint32_t player_id = m_lobbyManager.login(login->username);
                        m_socketToPlayerId[socket] = player_id;
                        AckLoginPacket ack;
                        ack.header.type = PacketType::LOGIN;
                        ack.header.size = sizeof(AckLoginPacket);
                        ack.success = true;
                        ack.player_id = player_id;
                        std::vector<char> response(sizeof(AckLoginPacket));
                        memcpy(response.data(), &ack, sizeof(AckLoginPacket));
                        envoyerAClient(socket, response);
                        break;
                    }
                    case PacketType::CHAT_MESSAGE: {
                        ChatMessagePacket* chat = reinterpret_cast<ChatMessagePacket*>(buffer->data());
                        std::cout << "Chat: " << chat->message << std::endl;
                        break;
                    }
                    case PacketType::JOIN_LOBBY: {
                        JoinLobbyPacket* join = reinterpret_cast<JoinLobbyPacket*>(buffer->data());
                        std::cout << "Rejoindre lobby: " << join->lobby_id << std::endl;
                        auto it = m_socketToPlayerId.find(socket);
                        if (it == m_socketToPlayerId.end()) {
                            std::cout << "Erreur: Client non connecté (pas de player_id)" << std::endl;
                            AckJoinLobbyPacket ack;
                            ack.header.type = PacketType::JOIN_LOBBY;
                            ack.header.size = sizeof(AckJoinLobbyPacket);
                            ack.success = false;
                            ack.lobby_id = 0;
                            std::vector<char> response(sizeof(AckJoinLobbyPacket));
                            memcpy(response.data(), &ack, sizeof(AckJoinLobbyPacket));
                            envoyerAClient(socket, response);
                            break;
                        }
                        uint32_t player_id = it->second;
                        auto [success, lobby_id] = m_lobbyManager.joinLobby(player_id, join->lobby_id);
                        AckJoinLobbyPacket ack;
                        ack.header.type = PacketType::JOIN_LOBBY;
                        ack.header.size = sizeof(AckJoinLobbyPacket);
                        ack.success = success;
                        ack.lobby_id = lobby_id;
                        std::vector<char> response(sizeof(AckJoinLobbyPacket));
                        memcpy(response.data(), &ack, sizeof(AckJoinLobbyPacket));
                        envoyerAClient(socket, response);

                        if (success) {
                            for (auto& pair : m_lobbyToSockets) {
                                pair.second.erase(std::remove(pair.second.begin(), pair.second.end(), socket), pair.second.end());
                            }
                            m_lobbyToSockets[lobby_id].push_back(socket);

                            if (m_lobbyManager.shouldStartGame(lobby_id)) {
                                auto timer = std::make_shared<boost::asio::steady_timer>(m_contexteIO, std::chrono::seconds(3));
                                timer->async_wait([this, lobby_id, timer](const boost::system::error_code& ec) {
                                    if (!ec) {
                                        GameStartPacket start;
                                        start.header.type = PacketType::GAME_START;
                                        start.header.size = sizeof(GameStartPacket);
                                        start.lobby_id = lobby_id;
                                        std::vector<char> start_msg(sizeof(GameStartPacket));
                                        memcpy(start_msg.data(), &start, sizeof(GameStartPacket));
                                        for (auto& sock : m_lobbyToSockets[lobby_id]) {
                                            envoyerAClient(sock, start_msg);
                                        }
                                        std::cout << "Jeu démarré pour le lobby " << lobby_id << std::endl;
                                    }
                                });
                            }
                        }
                        break;
                    }
                    case PacketType::LOBBY_LIST: {
                        std::cout << "Demande liste lobbies" << std::endl;
                        auto lobbies = m_lobbyManager.getLobbyList();
                        LobbyListResponsePacket response;
                        response.header.type = PacketType::LOBBY_LIST;
                        response.header.size = sizeof(LobbyListResponsePacket);
                        response.num_lobbies = std::min(static_cast<uint8_t>(lobbies.size()), static_cast<uint8_t>(M_LOBBIES));
                        for (size_t i = 0; i < response.num_lobbies; ++i) {
                            response.lobbies[i].lobby_id = lobbies[i].id;
                            response.lobbies[i].player_count = lobbies[i].players.size();
                            response.lobbies[i].max_players = lobbies[i].max_players;
                        }
                        std::vector<char> msg(sizeof(LobbyListResponsePacket));
                        memcpy(msg.data(), &response, sizeof(LobbyListResponsePacket));
                        envoyerAClient(socket, msg);
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

void TCPServer::envoyerAClient(std::shared_ptr<tcp::socket> socket, const std::vector<char>& message) {
    if (socket->is_open()) {
        boost::asio::write(*socket, boost::asio::buffer(message));
    }
}
