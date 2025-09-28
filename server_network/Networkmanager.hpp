#pragma once

#include "protocol.hpp"
#include "concurent_queue.hpp"
#include <memory>

class NetworkManager {
public:
    static NetworkManager& getInstance();
    
    bool initialize(uint16_t tcp_port, uint16_t udp_port);
    void shutdown();
    
    bool hasInputs() const;
    bool popInput(InputPacket& input);
    
    void sendSnapshot(const SnapshotPacket& snapshot);
    
    uint32_t getConnectedPlayersCount() const;
    
private:
    NetworkManager() = default;
    ~NetworkManager() = default;
    
    class Impl;
    std::unique_ptr<Impl> m_impl;
};