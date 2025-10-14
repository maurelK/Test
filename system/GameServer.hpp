#ifndef GAMESERVER_HPP
#define GAMESERVER_HPP

#include "../rtype_engine/Orchestror.hpp"
#include "../Network/protocol.hpp"
#include "../Network/Networkmanager.hpp"

// INCLURE TOUS LES SYSTÈMES
#include "MovementSystem.hpp"
#include "HealthSystem.hpp"
#include "SpawnSystem.hpp"
#include "CollisionSystem.hpp"
#include "ProjectileSystem.hpp"

#include <unordered_map>
#include <memory>
#include <vector>

class GameServer {
private:
    std::unique_ptr<Orchestror> orchestr;
    
    // Déclarer les systèmes APRÈS avoir inclus leurs headers
    std::shared_ptr<MovementSystem> moveSys;
    std::shared_ptr<HealthSystem> healthSys;
    std::shared_ptr<SpawnSystem> spawnSys;
    std::shared_ptr<CollisionSystem> collisionSys;
    std::shared_ptr<ProjectileSystem> projectileSys;

    std::unordered_map<Entity, EntityData> lastSnapshots;
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