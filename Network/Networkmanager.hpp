// server/network/NetworkManager.hpp
#pragma once

#include "protocol.hpp"
#include "concurent_queue.hpp"
#include "tcp_server.hpp"
#include "udp_server.hpp"
#include "LobbyManager.hpp"
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

    uint32_t login(const std::string& username);
    std::vector<Lobby> getLobbyList();
    std::pair<bool, uint32_t> joinLobby(uint32_t player_id, uint32_t lobby_id);
    uint32_t createLobby(uint8_t max_players = 4);

private:
    NetworkManager();
    ~NetworkManager() = default;
    
    NetworkManager(const NetworkManager&) = delete;
    NetworkManager& operator=(const NetworkManager&) = delete;
    
    TCPServer m_serveurTCP;
    UDPServer m_serveurUDP;
    ConcurrentQueue m_inputQueue;
    LobbyManager m_lobbyManager;
};
