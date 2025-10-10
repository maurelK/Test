// server/network/NetworkManager.hpp
#pragma once

#include "protocol.hpp"
#include "concurent_queue.hpp"
#include "tcp_server.hpp"
#include "udp_server.hpp"
#include <memory>

class NetworkManager {
public:
    static NetworkManager& getInstance();
    
    bool initialize(uint16_t tcp_port = 9090, uint16_t udp_port = 9091);
    void shutdown();
    
    bool hasInputs() const;
    bool popInput(InputPacket& input);
    void sendSnapshot(const SnapshotPacket& snapshot);
    uint32_t getConnectedPlayersCount() const;

private:
    NetworkManager();
    ~NetworkManager() = default;
    
    NetworkManager(const NetworkManager&) = delete;
    NetworkManager& operator=(const NetworkManager&) = delete;
    
    TCPServer m_serveurTCP;
    UDPServer m_serveurUDP;
    ConcurrentQueue m_inputQueue;
};