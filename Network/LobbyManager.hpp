#pragma once

#include "protocol.hpp"
#include <map>
#include <vector>
#include <string>
#include <mutex>

struct Lobby {
    uint32_t id;
    std::vector<uint32_t> players;
    uint8_t max_players;
    bool game_started;
};

class LobbyManager {
    public:
        LobbyManager();
        ~LobbyManager() = default;

        uint32_t login(const std::string& username);

        std::vector<Lobby> getLobbyList();

        std::pair<bool, uint32_t> joinLobby(uint32_t player_id, uint32_t lobby_id);

        bool shouldStartGame(uint32_t lobby_id);

        uint32_t createLobby(uint8_t max_players = 4);

    private:
        std::map<uint32_t, std::string> m_players;
        std::vector<Lobby> m_lobbies;
        uint32_t m_nextPlayerId;
        uint32_t m_nextLobbyId;
        std::mutex m_mutex;
};
