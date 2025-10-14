/*
** EPITECH PROJECT, 2025
** R-Type GameServer Header (ECS)
*/

#pragma once

#include "../rtype_engine/EntityManager.hpp"
#include "../rtype_engine/SystemManager.hpp"
#include "../rtype_engine/Components.hpp"
#include "../rtype_engine/Component_storage.hpp"
#include "MovementSystem.hpp"
#include "HealthSystem.hpp"
#include "SpawnSystem.hpp"
#include "CollisionSystem.hpp"
#include "ProjectileSystem.hpp"
#include "../Network/Networkmanager.hpp"
#include "../Network/protocol.hpp"

#include <vector>
#include <memory>

struct GameEntityData {
    Entity id;
    float x;
    float y;
};

class GameServer {
public:
    GameServer();

    void initialize();
    void update(float dt);
    void sendSnapshots();

    Entity spawnEnemy(float x, float y);
    Entity spawnProjectileFrom(Entity owner, float dx, float dy, int damage = 10, float life = 3.0f);

private:
    EntityManager entityManager;
    SystemManager systemManager;

    ComponentArray<Position> positions;
    ComponentArray<Velocity> velocities;
    ComponentArray<Health> healths;
    ComponentArray<Projectile> projectiles;

    std::shared_ptr<MovementSystem> moveSys;
    std::shared_ptr<HealthSystem> healthSys;
    std::shared_ptr<SpawnSystem> spawnSys;
    std::shared_ptr<CollisionSystem> collisionSys;
    std::shared_ptr<ProjectileSystem> projectileSys;

    uint64_t tick = 0;
    
    std::unordered_map<Entity, Position> previousPositions;
    uint32_t fullSnapshotInterval = 60;
};
