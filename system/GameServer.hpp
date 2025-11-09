#pragma once

#include "../engine/include/ECS_architecture/Orchestror.hpp"
#include "MovementSystem.hpp"
#include "HealthSystem.hpp"
#include "CollisionSystem.hpp"
#include "SpawnSystem.hpp"
#include "ProjectileSystem.hpp"
#include "EnemySystem.hpp"
#include "../Network/Networkmanager.hpp"
#include "../Network/protocol.hpp"
#include <unordered_map>
#include <vector>
#include <memory>
#include <iostream>

struct PlayerGameStats {
    int score = 0;
    int lives = 3;
    int enemiesKilled = 0;
};

class GameServer {
private:
    Orchestror orchestror;

    std::shared_ptr<MovementSystem> moveSys;
    std::shared_ptr<HealthSystem> healthSys;
    std::shared_ptr<CollisionSystem> collisionSys;
    std::shared_ptr<SpawnSystem> spawnSys;
    std::shared_ptr<ProjectileSystem> projectileSys;
    std::shared_ptr<EnemySystem> enemySys;

    std::unordered_map<uint32_t, Entity> playerEntities;
    std::unordered_map<uint32_t, PlayerGameStats> playerStats;

    std::unordered_map<Entity, Position> lastEntityPositions; // suivi pour snapshots delta

    uint64_t tick = 0;

public:
    GameServer();
    void run();
    void initialize();
    void update(float dt);

    Entity spawnPlayer(uint32_t playerId, float x, float y);
    Entity spawnEnemy(float x, float y);
    Entity spawnProjectileFrom(Entity owner, float dx, float dy, int damage, float life);

    void handleInput(uint32_t playerId, float moveX, float moveY, bool shoot);

    void sendDeltaSnapshots();

    void onEnemyKilled(uint32_t killerPlayerId);
    void onPlayerHit(uint32_t playerId);
    PlayerGameStats& getPlayerStats(uint32_t playerId);

    std::vector<EntityData> getEntitiesState() const;
};
