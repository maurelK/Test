#pragma once
#include "protocol.hpp"
#include <boost/asio.hpp>
#include <atomic>
#include <thread>
#include <string>
#include <memory>
#include <mutex>
#include <queue>
#include <optional>

using boost::asio::ip::udp;
using boost::asio::ip::tcp;

class NetworkClient {
public:
    NetworkClient(const std::string& host, unsigned short port);
    ~NetworkClient();

    bool start();
    void stop();

    //  TCP
    bool sendLogin(const std::string& username);
    bool joinLobby(uint32_t lobbyId);
    std::optional<PacketType> listenForServerEvents();

    // UDP
    void sendInput(const InputPacket& input);
    bool pollSnapshot(SnapshotPacket& snapshot);

    uint32_t getPlayerId() const { return m_playerId; }

private:
    boost::asio::io_context m_context;

    // UDP
    udp::socket m_socket;
    udp::endpoint m_serverEndpoint;

    // TCP
    std::unique_ptr<tcp::socket> m_tcpSocket;

    std::atomic<bool> m_running{false};
    std::thread m_thread;
    uint32_t m_playerId = 0;
};
