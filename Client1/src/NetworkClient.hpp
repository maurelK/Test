#pragma once

#include <boost/asio.hpp>
#include <thread>
#include <memory>
#include <optional>
#include "../../Network/protocol.hpp"

using boost::asio::ip::udp;
using boost::asio::ip::tcp;

class NetworkClient {
public:
    // Constructeur avec ports TCP et UDP
    NetworkClient(const std::string& host, unsigned short tcpPort = 9090, unsigned short udpPort = 9091);
    ~NetworkClient();

    bool start();
    void stop();

    bool sendLogin(const std::string& username);
    bool joinLobby(uint32_t lobbyId);

    void sendInput(const InputPacket& input);
    bool pollSnapshot(SnapshotPacket& snapshot);

    std::optional<PacketType> listenForServerEvents();

    uint32_t getPlayerId() const { return m_playerId; }

private:
    boost::asio::io_context m_context;

    // Socket UDP pour envoyer les inputs et recevoir les snapshots
    udp::socket m_socket;
    udp::endpoint m_serverUDPEndpoint;

    // Socket TCP pour login et lobby
    std::unique_ptr<tcp::socket> m_tcpSocket;
    tcp::endpoint m_serverTCPEndpoint;

    std::thread m_thread;
    bool m_running = false;
    uint32_t m_playerId = 0;
};