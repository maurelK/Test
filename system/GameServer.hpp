#ifndef GAMESERVER_HPP
#define GAMESERVER_HPP

#include "../rtype_engine/Orchestror.hpp"
#include "../Network/protocol.hpp"
#include "../Network/Networkmanager.hpp"

#include <unordered_map>
#include <memory>
#include <vector>

// Supprimer cette structure, on utilisera directement les composants
// struct Snapshot {
//     Position pos;
//     Velocity vel;
//     Health hp;
//     bool operator!=(const Snapshot& other) const {
//         return pos != other.pos || vel != other.vel || hp != other.hp;
//     }
// };

class GameServer {
private:
    std::unique_ptr<Orchestror> orchestr;
    
    // SUPPRIMER les ComponentStorage manuels!
    // ComponentStorage<Position> positions;  ← À SUPPRIMER
    // ComponentStorage<Velocity> velocities; ← À SUPPRIMER  
    // ComponentStorage<Health> healths;      ← À SUPPRIMER
    // ComponentStorage<Projectile> projectiles; ← À SUPPRIMER

    // SUPPRIMER SystemManager manuel
    // SystemManager systemManager; ← À SUPPRIMER

    // Garder les systèmes mais ils utiliseront Orchestror
    std::shared_ptr<MovementSystem> moveSys;
    std::shared_ptr<HealthSystem> healthSys;
    std::shared_ptr<SpawnSystem> spawnSys;
    std::shared_ptr<CollisionSystem> collisionSys;
    std::shared_ptr<ProjectileSystem> projectileSys;

    std::unordered_map<Entity, EntityData> lastSnapshots; // Stocker EntityData directement
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