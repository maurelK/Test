#pragma once

#include <cstdint>

#pragma pack(push, 1)

enum class PacketType : uint8_t {
    LOGIN = 0,
    JOIN_LOBBY,
    CHAT_MESSAGE,
    LOBBY_LIST,
    INPUT,
    SNAPSHOT,
    GAME_START
};

struct PacketHeader {
    PacketType type;
    uint16_t size;
};

struct LoginPacket {
    static constexpr PacketType Type = PacketType::LOGIN;
    PacketHeader header;
    char username[32];
};

struct JoinLobbyPacket {
    static constexpr PacketType Type = PacketType::JOIN_LOBBY;
    PacketHeader header;
    uint32_t lobby_id;
};

struct ChatMessagePacket {
    static constexpr PacketType Type = PacketType::CHAT_MESSAGE;
    PacketHeader header;
    char message[256];
};

struct LobbyListPacket {
    static constexpr PacketType Type = PacketType::LOBBY_LIST;
    PacketHeader header;
    uint32_t lobby_id;
    uint8_t player_count;
};

struct InputPacket {
    static constexpr PacketType Type = PacketType::INPUT;
    PacketHeader header;
    uint32_t player_id;
    float move_x;
    float move_y;
    bool shoot;
};

#define M_ENTITIES 100

struct EntityData {
    uint32_t id;
    float x, y;
};

struct PlayerStats {
    uint32_t player_id;
    int score;
    int lives;
    int enemies_killed;
};

struct SnapshotPacket {
    static constexpr PacketType Type = PacketType::SNAPSHOT;
    PacketHeader header;
    uint32_t tick;
    uint16_t num_entities;
    EntityData entities[M_ENTITIES];
    uint8_t num_players;
    PlayerStats player_stats[4];

};

struct AckLoginPacket {
    static constexpr PacketType Type = PacketType::LOGIN;
    PacketHeader header;
    bool success;
    uint32_t player_id;
};

struct LobbyInfo {
    uint32_t lobby_id;
    uint8_t player_count;
    uint8_t max_players;
};

#define M_LOBBIES 10

struct LobbyListResponsePacket {
    static constexpr PacketType Type = PacketType::LOBBY_LIST;
    PacketHeader header;
    uint8_t num_lobbies;
    LobbyInfo lobbies[M_LOBBIES];
};

struct AckJoinLobbyPacket {
    static constexpr PacketType Type = PacketType::JOIN_LOBBY;
    PacketHeader header;
    bool success;
    uint32_t lobby_id;
};

struct GameStartPacket {
    static constexpr PacketType Type = PacketType::GAME_START;
    PacketHeader header;
    uint32_t lobby_id;
};

#pragma pack(pop)
