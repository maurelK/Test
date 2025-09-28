#pragma once

#include <cstdint>

#pragma pack(push, 1)

enum class PacketType : uint8_t {
    LOGIN = 0,
    JOIN_LOBBY,
    CHAT_MESSAGE,
    LOBBY_LIST,
    INPUT,
    SNAPSHOT
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

#define MAX_ENTITIES 100

struct EntityData {
    uint32_t id;
    float x, y;
};

struct SnapshotPacket {
    static constexpr PacketType Type = PacketType::SNAPSHOT;
    PacketHeader header;
    uint32_t tick;
    uint16_t num_entities;
    EntityData entities[MAX_ENTITIES];
};

#pragma pack(pop)