#include "NetworkClient.hpp"
#include <iostream>
#include <cstring>
#include <boost/asio.hpp>
#include <optional>

NetworkClient::NetworkClient(const std::string& host, unsigned short tcpPort, unsigned short udpPort)
    : m_socket(m_context, udp::endpoint(udp::v4(), 0))
{
    m_serverTCPEndpoint = tcp::endpoint(boost::asio::ip::make_address(host), tcpPort);
    m_serverUDPEndpoint = udp::endpoint(boost::asio::ip::make_address(host), udpPort);
}

NetworkClient::~NetworkClient() {
    stop();
}

bool NetworkClient::start() {
    try {
        if (!m_socket.is_open())
            m_socket.open(udp::v4());

        std::cout << "[Client] Port local UDP: " << m_socket.local_endpoint().port() << std::endl;
        m_running = true;
        return true;
    } catch (std::exception& e) {
        std::cerr << "[Client] Erreur connexion UDP: " << e.what() << std::endl;
        return false;
    }
}

void NetworkClient::stop() {
    m_running = false;
    if (m_thread.joinable()) m_thread.join();

    if (m_tcpSocket && m_tcpSocket->is_open()) {
        boost::system::error_code ec;
        m_tcpSocket->close(ec);
    }

    if (m_socket.is_open()) {
        boost::system::error_code ec;
        m_socket.close(ec);
    }

    std::cout << "[Client] Arrêt du client réseau." << std::endl;
}

bool NetworkClient::sendLogin(const std::string& username) {
    try {
        // Création et connexion du socket TCP si nécessaire
        if (!m_tcpSocket) {
            m_tcpSocket = std::make_unique<tcp::socket>(m_context);
            tcp::endpoint tcpEndpoint(boost::asio::ip::make_address("127.0.0.1"), 9090); // adresse et port serveur TCP
            m_tcpSocket->connect(tcpEndpoint);
        }

        // Préparation du paquet de login
        LoginPacket packet{};
        packet.header.type = PacketType::LOGIN;
        packet.header.size = sizeof(LoginPacket);
        std::snprintf(packet.username, sizeof(packet.username), "%s", username.c_str());

        // Envoi du login
        boost::asio::write(*m_tcpSocket, boost::asio::buffer(&packet, sizeof(packet)));
        std::cout << "[Client] Login envoyé: " << username << std::endl;

        // Lecture de l'ACK
        AckLoginPacket ack{};
        boost::asio::read(*m_tcpSocket, boost::asio::buffer(&ack, sizeof(ack)));

        if (ack.success) {
            m_playerId = ack.player_id;
            std::cout << "[Client] Login ACK reçu - PlayerID: " << m_playerId << std::endl;
            return true;
        } else {
            std::cerr << "[Client] Login échoué (ACK invalide)" << std::endl;
            return false;
        }
    } catch (std::exception& e) {
        std::cerr << "[Client] Erreur TCP login: " << e.what() << std::endl;
        return false;
    }
}

bool NetworkClient::joinLobby(uint32_t lobbyId) {
    try {
        if (!m_tcpSocket || !m_tcpSocket->is_open()) return false;

        JoinLobbyPacket packet{};
        packet.header.type = PacketType::JOIN_LOBBY;
        packet.header.size = sizeof(packet);
        packet.lobby_id = lobbyId;

        boost::asio::write(*m_tcpSocket, boost::asio::buffer(&packet, sizeof(packet)));
        std::cout << "[Client] Rejoint le lobby ID: " << lobbyId << std::endl;

        AckJoinLobbyPacket ack{};
        boost::asio::read(*m_tcpSocket, boost::asio::buffer(&ack, sizeof(ack)));

        if (ack.success) {
            std::cout << "[Client] JoinLobby réussi (Lobby " << ack.lobby_id << ")\n";
            return true;
        } else {
            std::cerr << "[Client] JoinLobby échoué.\n";
        }
    } catch (std::exception& e) {
        std::cerr << "[Client] Erreur JoinLobby: " << e.what() << std::endl;
    }
    return false;
}

std::optional<PacketType> NetworkClient::listenForServerEvents() {
    if (!m_tcpSocket || !m_tcpSocket->is_open()) return std::nullopt;

    try {
        boost::system::error_code ec;
        size_t available = m_tcpSocket->available(ec);
        if (ec || available < sizeof(PacketHeader)) return std::nullopt;

        PacketHeader header;
        size_t bytes = boost::asio::read(*m_tcpSocket, boost::asio::buffer(&header, sizeof(header)), ec);
        if (ec || bytes < sizeof(PacketHeader)) return std::nullopt;

        if (header.type == PacketType::GAME_START) {
            std::cout << "[Client] GAME_START reçu du serveur.\n";
            return PacketType::GAME_START;
        }
    } catch (std::exception& e) {
        std::cerr << "[Client] Erreur lecture TCP: " << e.what() << std::endl;
    }
    return std::nullopt;
}

bool NetworkClient::pollSnapshot(SnapshotPacket& snapshot) {
    if (!m_socket.is_open()) return false;

    boost::system::error_code ec;
    size_t available = m_socket.available(ec);
    if (ec || available < sizeof(PacketHeader)) return false;

    std::vector<char> buffer(sizeof(SnapshotPacket));
    size_t received = m_socket.receive(boost::asio::buffer(buffer), 0, ec);

    if (!ec && received >= sizeof(PacketHeader)) {
        PacketHeader* header = reinterpret_cast<PacketHeader*>(buffer.data());
        if (header->type == PacketType::SNAPSHOT) {
            snapshot = *reinterpret_cast<SnapshotPacket*>(buffer.data());
            return true;
        }
    }
    return false;
}

void NetworkClient::sendInput(const InputPacket& input) {
    try {
        m_socket.send_to(boost::asio::buffer(&input, sizeof(input)), m_serverUDPEndpoint);
    } catch (std::exception& e) {
        std::cerr << "[Client] Erreur envoi UDP input: " << e.what() << std::endl;
    }
}