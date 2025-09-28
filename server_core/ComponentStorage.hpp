#pragma once
#include <unordered_map>
#include <cstdint>

using EntityID = uint32_t;
inline constexpr EntityID INVALID_ENTITY = UINT32_MAX;

struct Position { float x = 0.f, y = 0.f; };
struct Velocity { float dx = 0.f, dy = 0.f; };
struct Health { int hp = 100; };
struct PlayerTag { int id = 0; };

class ComponentStorage {
private:
    std::unordered_map<EntityID, Position> positions;
    std::unordered_map<EntityID, Velocity> velocities;
    std::unordered_map<EntityID, Health> healths;
    std::unordered_map<EntityID, PlayerTag> playerTags;

public:
    void addPosition(EntityID id, const Position& pos);
    void addVelocity(EntityID id, const Velocity& vel);
    void addHealth(EntityID id, const Health& h);
    void addPlayerTag(EntityID id, const PlayerTag& tag);

    Position* getPosition(EntityID id);
    const Position* getPosition(EntityID id) const;

    Velocity* getVelocity(EntityID id);
    const Velocity* getVelocity(EntityID id) const;

    Health* getHealth(EntityID id);
    const Health* getHealth(EntityID id) const;

    PlayerTag* getPlayerTag(EntityID id);
    const PlayerTag* getPlayerTag(EntityID id) const;

    const std::unordered_map<EntityID, Position>& getAllPositions() const;
};
