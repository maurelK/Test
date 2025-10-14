/*
** EPITECH PROJECT, 2025
** R-Type GameServer Implementation (ECS)
*/

#include "GameServer.hpp"
#include <iostream>
#include <cstring>

GameServer::GameServer()
{
    moveSys = systemManager.registerSystem<MovementSystem>(positions, velocities);
    healthSys = systemManager.registerSystem<HealthSystem>(healths, entityManager);
    spawnSys = systemManager.registerSystem<SpawnSystem>(entityManager, positions, velocities, healths);
    collisionSys = systemManager.registerSystem<CollisionSystem>(positions, healths);
    projectileSys = std::make_shared<ProjectileSystem>(entityManager, positions, velocities, projectiles);
}

void GameServer::initialize()
{
    Entity e1 = spawnEnemy(100.f, 50.f);
    Entity e2 = spawnEnemy(200.f, 60.f);

    moveSys->addEntity(e1);
    moveSys->addEntity(e2);
    healthSys->addEntity(e1);
    healthSys->addEntity(e2);
    collisionSys->addEntity(e1);
    collisionSys->addEntity(e2);
}

void GameServer::update(float dt)
{
    systemManager.update(dt);
    projectileSys->update(dt);
    sendSnapshots();
}

void GameServer::sendSnapshots()
{
    if (NetworkManager::getInstance().getConnectedPlayersCount() == 0) {
        tick++;
        return;
    }

    SnapshotPacket snapshot;
    snapshot.header.type = PacketType::SNAPSHOT;
    snapshot.tick = tick;
    snapshot.num_entities = 0;

    bool sendFullSnapshot = (tick % fullSnapshotInterval == 0);
    
    if (sendFullSnapshot) {

        std::cout << "[Snapshot] FULL snapshot at tick=" << tick << std::endl;
        
        for (auto e : entityManager.getLivingEntities()) {
            if (positions.hasData(e) && snapshot.num_entities < M_ENTITIES) {
                auto& pos = positions.getData(e);
                snapshot.entities[snapshot.num_entities].id = e;
                snapshot.entities[snapshot.num_entities].x = pos.x;
                snapshot.entities[snapshot.num_entities].y = pos.y;
                snapshot.num_entities++;
                previousPositions[e] = pos;
            }
        }
    } else {
        for (auto e : entityManager.getLivingEntities()) {
            if (positions.hasData(e) && snapshot.num_entities < M_ENTITIES) {
                auto& currentPos = positions.getData(e);
                
                auto it = previousPositions.find(e);
                bool isNew = (it == previousPositions.end());
                bool hasChanged = isNew || (it->second != currentPos);
                
                if (hasChanged) {
                    snapshot.entities[snapshot.num_entities].id = e;
                    snapshot.entities[snapshot.num_entities].x = currentPos.x;
                    snapshot.entities[snapshot.num_entities].y = currentPos.y;
                    snapshot.num_entities++;
                    
                    previousPositions[e] = currentPos;
                }
            }
        }
        std::vector<Entity> toRemove;
        for (auto& [cachedEntity, cachedPos] : previousPositions) {
            bool stillAlive = false;
            for (auto e : entityManager.getLivingEntities()) {
                if (e == cachedEntity) {
                    stillAlive = true;
                    break;
                }
            }
            if (!stillAlive) {
                toRemove.push_back(cachedEntity);
            }
        }
        
        for (auto e : toRemove) {
            previousPositions.erase(e);
        }
    }
    if (snapshot.num_entities == 0) {
        tick++;
        return;
    }

    snapshot.header.size = sizeof(PacketHeader) + sizeof(uint32_t) + sizeof(uint16_t) + 
                           (snapshot.num_entities * sizeof(EntityData));

    NetworkManager::getInstance().sendSnapshot(snapshot);
    
    if (sendFullSnapshot) {
        std::cout << "[Snapshot] Tick=" << tick << " FULL - Entities=" << snapshot.num_entities << " sent" << std::endl;
    } else {
        std::cout << "[Snapshot] Tick=" << tick << " DELTA - Changed entities=" << snapshot.num_entities << " sent" << std::endl;
    }
    
    tick++;
}

Entity GameServer::spawnEnemy(float x, float y)
{
    Entity e = spawnSys->spawnEnemy(x, y);
    moveSys->addEntity(e);
    healthSys->addEntity(e);
    collisionSys->addEntity(e);
    return e;
}

Entity GameServer::spawnProjectileFrom(Entity owner, float dx, float dy, int damage, float life)
{
    if (!positions.hasData(owner))
        return static_cast<Entity>(-1);

    auto pos = positions.getData(owner);
    Entity p = projectileSys->spawnProjectile(owner, pos.x, pos.y, dx, dy, damage, life);
    moveSys->addEntity(p);
    return p;
}
