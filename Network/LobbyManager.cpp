#include "LobbyManager.hpp"
#include <algorithm>

LobbyManager::LobbyManager() : m_nextPlayerId(1), m_nextLobbyId(1)
{
    createLobby(4);
    createLobby(4);
}

uint32_t LobbyManager::login(const std::string& username)
{
    std::lock_guard<std::mutex> lock(m_mutex);
    uint32_t player_id = m_nextPlayerId++;
    m_players[player_id] = username;
    return player_id;
}

std::vector<Lobby> LobbyManager::getLobbyList()
{
    std::lock_guard<std::mutex> lock(m_mutex);
    return m_lobbies;
}

std::pair<bool, uint32_t> LobbyManager::joinLobby(uint32_t player_id, uint32_t lobby_id)
{
    std::lock_guard<std::mutex> lock(m_mutex);
    auto it = std::find_if(m_lobbies.begin(), m_lobbies.end(),
        [lobby_id](const Lobby& l) { return l.id == lobby_id; });
    if (it != m_lobbies.end() && it->players.size() < it->max_players) {
        for (auto& lobby : m_lobbies) {
            lobby.players.erase(std::remove(lobby.players.begin(), lobby.players.end(), player_id), lobby.players.end());
        }
        it->players.push_back(player_id);
        return {true, lobby_id};
    }
    return {false, 0};
}

uint32_t LobbyManager::createLobby(uint8_t max_players)
{
    std::lock_guard<std::mutex> lock(m_mutex);
    Lobby newLobby;
    newLobby.id = m_nextLobbyId++;
    newLobby.max_players = max_players;
    newLobby.game_started = false;
    m_lobbies.push_back(newLobby);
    return newLobby.id;
}

bool LobbyManager::shouldStartGame(uint32_t lobby_id)
{
    std::lock_guard<std::mutex> lock(m_mutex);
    auto it = std::find_if(m_lobbies.begin(), m_lobbies.end(),
        [lobby_id](const Lobby& l) { return l.id == lobby_id; });
    if (it != m_lobbies.end() && !it->game_started && it->players.size() >= 2) {
        it->game_started = true;
        return true;
    }
    return false;
}
