#include "ComponentStorage.hpp"

// Ajout de composants
void ComponentStorage::addPosition(EntityID id, const Position& pos) {
    positions[id] = pos;
}
void ComponentStorage::addVelocity(EntityID id, const Velocity& vel) {
    velocities[id] = vel;
}
void ComponentStorage::addHealth(EntityID id, const Health& h) {
    healths[id] = h;
}
void ComponentStorage::addPlayerTag(EntityID id, const PlayerTag& tag) {
    playerTags[id] = tag;
}

// Accès aux composants
Position* ComponentStorage::getPosition(EntityID id) {
    auto it = positions.find(id);
    return it != positions.end() ? &it->second : nullptr;
}
const Position* ComponentStorage::getPosition(EntityID id) const {
    auto it = positions.find(id);
    return it != positions.end() ? &it->second : nullptr;
}

Velocity* ComponentStorage::getVelocity(EntityID id) {
    auto it = velocities.find(id);
    return it != velocities.end() ? &it->second : nullptr;
}
const Velocity* ComponentStorage::getVelocity(EntityID id) const {
    auto it = velocities.find(id);
    return it != velocities.end() ? &it->second : nullptr;
}

Health* ComponentStorage::getHealth(EntityID id) {
    auto it = healths.find(id);
    return it != healths.end() ? &it->second : nullptr;
}
const Health* ComponentStorage::getHealth(EntityID id) const {
    auto it = healths.find(id);
    return it != healths.end() ? &it->second : nullptr;
}

PlayerTag* ComponentStorage::getPlayerTag(EntityID id) {
    auto it = playerTags.find(id);
    return it != playerTags.end() ? &it->second : nullptr;
}
const PlayerTag* ComponentStorage::getPlayerTag(EntityID id) const {
    auto it = playerTags.find(id);
    return it != playerTags.end() ? &it->second : nullptr;
}

// Accès à toutes les positions
const std::unordered_map<EntityID, Position>& ComponentStorage::getAllPositions() const {
    return positions;
}
