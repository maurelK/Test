/*
** EPITECH PROJECT, 2025
** R-Type GameServer Implementation
*/

#include "GameServer.hpp"
#include <iostream>
#include <thread>
#include "../Network/protocol.hpp"

GameServer::GameServer() {

    moveSys = systemManager.registerSystem<MovementSystem>(positions, velocities);
    healthSys = systemManager.registerSystem<HealthSystem>(healths);
    spawnSys = systemManager.registerSystem<SpawnSystem>(entityManager, positions, velocities, healths);
    collisionSys = systemManager.registerSystem<CollisionSystem>(positions, healths);
    projectileSys = std::make_shared<ProjectileSystem>(entityManager, positions, velocities, projectiles, healths);
}

void GameServer::initialize() {

    Entity e1 = spawnEnemy(100.f, 50.f);
    Entity e2 = spawnEnemy(200.f, 60.f);


    moveSys->addEntity(e1);
    moveSys->addEntity(e2);
    healthSys->addEntity(e1);
    healthSys->addEntity(e2);
    collisionSys->addEntity(e1);
    collisionSys->addEntity(e2);
}

void GameServer::update(float dt) {
    InputPacket input;

    while(NetworkManager::getInstance().popInput(input)) {
        handleInput(input.player_id, input.move_x, input.move_y, input.shoot);
    }
    systemManager.update(dt);
    projectileSys->update(dt);
    sendSnapshots();
}

void GameServer::sendSnapshots() {
    std::vector<EntityData> entities;
    for (auto e : entityManager.getLivingEntities()) {
        if (positions.hasData(e)) {
            auto& pos = positions.getData(e);
            entities.push_back({e, pos.x, pos.y});
        }
    }
        if (entities.empty()) return;

    SnapshotPacket snap;
    snap.header.type = PacketType::SNAPSHOT;
    snap.header.size = sizeof(SnapshotPacket);
    snap.tick = tick++;
    snap.num_entities = entities.size();

    std::memcpy(snap.entities, entities.data(), sizeof(EntityData) * entities.size());
    NetworkManager::getInstance().sendSnapshot(snap);
       // bool changed = false;

        //if (positions.hasData(e) && velocities.hasData(e) && healths.hasData(e)) {
          //  auto& pos = positions.getData(e);
            //auto& vel = velocities.getData(e);
            //auto& hp  = healths.getData(e);

            //auto it = lastSnapshots.find(e);
            //if (it == lastSnapshots.end()) {
              //  changed = true;
           // } else {
             //   const Snapshot& last = it->second;
               // changed = (last.pos != pos) || (last.vel != vel) || (last.hp != hp);
            //}

            //if (changed) {
              //  Snapshot snap{ pos, vel, hp };
               // lastSnapshots[e] = snap;

               // std::cout << "Entity " << e
                 //         << " | Position(" << pos.x << "," << pos.y << ")"
                   //       << " | Velocity(" << vel.dx << "," << vel.dy << ")"
                    //      << " | HP: " << hp.hp << "\n";

                // Ici : envoyer le snapshot via ton NetworkManager
           // }
        //}
        // Vérification des projectiles
        /*else if (positions.hasData(e) && velocities.hasData(e) && projectiles.hasData(e)) {
            auto& pos  = positions.getData(e);
            auto& vel  = velocities.getData(e);
            auto& proj = projectiles.getData(e);

            auto it = lastSnapshots.find(e);
            if (it == lastSnapshots.end() || true) {
                std::cout << "Projectile " << e
                          << " | Position(" << pos.x << "," << pos.y << ")"
                          << " | Velocity(" << vel.dx << "," << vel.dy << ")"
                          << " | Owner: " << proj.owner
                          << " | Life: " << proj.life << "\n";
            }
        }
    }*/
}


Entity GameServer::spawnEnemy(float x, float y) {
    Entity e = spawnSys->spawnEnemy(x, y);
    moveSys->addEntity(e);
    healthSys->addEntity(e);
    collisionSys->addEntity(e);
    return e;
}

Entity GameServer::spawnProjectileFrom(Entity owner, float dx, float dy, int damage, float life) {
    if (!positions.hasData(owner))
        return static_cast<Entity>(-1);

    auto pos = positions.getData(owner);
    Entity p = projectileSys->spawnProjectile(owner, pos.x, pos.y, dx, dy, damage, life);

    moveSys->addEntity(p);

    return p;
}


void GameServer::handleInput(uint32_t playerId, float moveX, float moveY, bool shoot)
{
    if (!positions.hasData(playerId)) {
        Entity e = spawnEnemy(50.f + playerId * 30, 50.f);
        std::cout << "Nouveau joueur connecté : ID " << playerId << " (entité " << e << ")\n";
    }

    if (velocities.hasData(playerId)) {
        auto& vel = velocities.getData(playerId);
        vel.dx = moveX * 100.f;
        vel.dy = moveY * 100.f;
    }
    if (shoot) {
        spawnProjectileFrom(playerId, 200.f, 0.f);
    }
}

std::vector<EntityData> GameServer::getEntitiesState() const
{
    std::vector<EntityData> entities;

    for (auto e : entityManager.getLivingEntities()) {
        if (positions.hasData(e)) {
            auto& pos = positions.getData(e);
            entities.push_back({ e, pos.x, pos.y });
        }
    }

    return entities;
}
