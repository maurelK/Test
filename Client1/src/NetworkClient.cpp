#include "NetworkClient.hpp"

#include <iostream>

NetworkClient::NetworkClient(const std::string& host, unsigned short port) 
        : m_socket(m_context, udp::endpoint(udp::v4(), 0))
        {
            m_serverEndpoint = udp::endpoint(boost::asio::ip::make_address(host), port);
        }


NetworkClient::~NetworkClient() {
    stop();
}

bool NetworkClient::start() {
    try {
        if (m_socket.is_open()) {
            m_socket.close();
        }

        m_socket.open(boost::asio::ip::udp::v4());
        m_socket.connect(m_serverEndpoint);

        std::cout << "[Client] Connecté au serveur " 
                  << m_serverEndpoint.address().to_string() 
                  << ":" << m_serverEndpoint.port() << std::endl;
        return true;

    } catch (std::exception& e) {
        std::cerr << "[Client] Erreur connexion: " << e.what() << std::endl;
        return false;
    }
}



void NetworkClient::stop() {
    m_running = false;

    if(m_thread.joinable()) m_thread.join();
    m_socket.close();
}

void NetworkClient::sendInput(const InputPacket& input) {
    try
    {
        m_socket.send_to(boost::asio::buffer(&input, sizeof(input)), m_serverEndpoint);
    } catch(const std::exception& e) {
    std::cerr << "[Client] Erreur envoie de l'input: "<< e.what() << std::endl;
    }
}

bool NetworkClient::pollSnapshot(SnapshotPacket& snapshot) {
    if (!m_socket.is_open()) 
        return false;

    boost::system::error_code ec;

    size_t available = m_socket.available(ec);
    if(ec || available < sizeof(PacketHeader))
    return false;

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
