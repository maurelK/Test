/*
** EPITECH PROJECT, 2025
** fe
** File description:
** fe
*/

#ifndef GAMESERVER_HPP
#define GAMESERVER_HPP

#include "../rtype_engine/EntityManager.hpp"
//#include "../Client1/.clang-format"
#include "../rtype_engine/Components.hpp"
#include "../rtype_engine/SystemManager.hpp"
#include "MovementSystem.hpp"
#include "HealthSystem.hpp"
#include "SpawnSystem.hpp"
#include "CollisionSystem.hpp"
#include "ProjectileSystem.hpp"
#include "../Network/protocol.hpp"
#include "../Network/Networkmanager.hpp"

#include <unordered_map>
#include <memory>
#include <vector>

struct Snapshot {
    Position pos;
    Velocity vel;
    Health hp;
    bool operator!=(const Snapshot& other) const {
        return pos != other.pos || vel != other.vel || hp != other.hp;
    }
};

class GameServer {
private:

    EntityManager entityManager;
    ComponentStorage<Position> positions;
    ComponentStorage<Velocity> velocities;
    ComponentStorage<Health> healths;
    ComponentStorage<Projectile> projectiles;

    SystemManager systemManager;

    std::shared_ptr<MovementSystem> moveSys;
    std::shared_ptr<HealthSystem> healthSys;
    std::shared_ptr<SpawnSystem> spawnSys;
    std::shared_ptr<CollisionSystem> collisionSys;
    std::shared_ptr<ProjectileSystem> projectileSys;

    std::unordered_map<Entity, Snapshot> lastSnapshots;
    uint64_t tick = 0;

public:
    GameServer();

    void initialize();
    void update(float dt);
    Entity spawnEnemy(float x, float y);
    Entity spawnProjectileFrom(Entity owner, float dx, float dy, int damage = 10, float life = 5.0f);

    void handleInput(uint32_t playerId, float moveX, float moveY, bool shoot);
    std::vector<EntityData> getEntitiesState() const;

private:
    void sendSnapshots();
};

#endif
