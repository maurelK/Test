#include "GameServer.hpp"
#include <chrono>
#include <thread>
#include <cstring>

GameServer::GameServer() {
    orchestror.init();

    // Enregistrer les composants
    orchestror.registerComponent<Position>();
    orchestror.registerComponent<Velocity>();
    orchestror.registerComponent<Health>();
    orchestror.registerComponent<Projectile>();

    // Enregistrer les systèmes
    moveSys = orchestror.registerSystem<MovementSystem>(orchestror);
    healthSys = orchestror.registerSystem<HealthSystem>(orchestror);
    collisionSys = orchestror.registerSystem<CollisionSystem>(orchestror);
    spawnSys = orchestror.registerSystem<SpawnSystem>(orchestror);
    projectileSys = orchestror.registerSystem<ProjectileSystem>(orchestror);
    enemySys = orchestror.registerSystem<EnemySystem>(orchestror);

    // Callback projectile tue un ennemi
    if (projectileSys) {
        projectileSys->setOnKillCallback([this](Entity killer, Entity) {
            onEnemyKilled(static_cast<uint32_t>(killer));
        });
    }
}

void GameServer::initialize() {
    spawnEnemy(100.f, 50.f);
    spawnEnemy(200.f, 60.f);
}

void GameServer::run() {
    if (!NetworkManager::getInstance().initialize(9090, 9091)) {
        std::cerr << "[GameServer] Impossible d'initialiser le réseau !" << std::endl;
        return;
    }

    initialize();

    const float dt = 0.016f; // 60 FPS
    while (true) {
        auto start = std::chrono::high_resolution_clock::now();
        update(dt);
        auto end = std::chrono::high_resolution_clock::now();
        auto elapsed = std::chrono::duration<float>(end - start).count();
        if (elapsed < dt)
            std::this_thread::sleep_for(std::chrono::duration<float>(dt - elapsed));
    }
}

void GameServer::update(float dt) {
    InputPacket input;
    while (NetworkManager::getInstance().popInput(input)) {
        handleInput(input.player_id, input.move_x, input.move_y, input.shoot);
    }

    orchestror.update(dt);
    if (projectileSys) projectileSys->update(dt);

    sendDeltaSnapshots();
}

Entity GameServer::spawnPlayer(uint32_t playerId, float x, float y) {
    Entity e = orchestror.createEntity();
    orchestror.addComponent(e, Position{x, y});
    orchestror.addComponent(e, Velocity{0.f, 0.f});
    orchestror.addComponent(e, Health{100});

    playerStats[playerId] = PlayerGameStats();
    playerEntities[playerId] = e;

    if (moveSys) moveSys->addEntity(e);
    if (healthSys) healthSys->addEntity(e);
    if (collisionSys) collisionSys->addEntity(e);

    lastEntityPositions[e] = orchestror.getComponent<Position>(e);
    return e;
}

Entity GameServer::spawnEnemy(float x, float y) {
    Entity e = spawnSys ? spawnSys->spawnEnemy(x, y) : orchestror.createEntity();
    orchestror.addComponent(e, Position{x, y});
    orchestror.addComponent(e, Velocity{-50.f, 0.f});
    orchestror.addComponent(e, Health{50});

    if (moveSys) moveSys->addEntity(e);
    if (healthSys) healthSys->addEntity(e);
    if (collisionSys) collisionSys->addEntity(e);
    if (enemySys) enemySys->addEntity(e);

    lastEntityPositions[e] = orchestror.getComponent<Position>(e);
    return e;
}

Entity GameServer::spawnProjectileFrom(Entity owner, float dx, float dy, int damage, float life) {
    if (!orchestror.hasComponent<Position>(owner)) return static_cast<Entity>(-1);

    auto& pos = orchestror.getComponent<Position>(owner);
    Entity p = projectileSys ? projectileSys->spawnProjectile(owner, pos.x, pos.y, dx, dy, damage, life) : orchestror.createEntity();

    orchestror.addComponent(p, Position{pos.x, pos.y});
    orchestror.addComponent(p, Velocity{dx, dy});
    orchestror.addComponent(p, Projectile{owner, damage, life});

    if (moveSys) moveSys->addEntity(p);
    lastEntityPositions[p] = orchestror.getComponent<Position>(p);
    return p;
}

void GameServer::handleInput(uint32_t playerId, float moveX, float moveY, bool shoot) {
    if (!playerEntities.count(playerId))
        spawnPlayer(playerId, 100.f + playerId * 80.f, 300.f);

    Entity e = playerEntities[playerId];
    if (orchestror.hasComponent<Velocity>(e)) {
        auto& vel = orchestror.getComponent<Velocity>(e);
        vel.dx = moveX * 100.f;
        vel.dy = moveY * 100.f;
    }

    if (shoot)
        spawnProjectileFrom(e, 200.f, 0.f, 10, 5.f);
}

void GameServer::sendDeltaSnapshots() {
    std::vector<EntityData> deltaEntities;
    for (auto e : orchestror.getEntities()) {
        if (!orchestror.hasComponent<Position>(e)) continue;
        auto& pos = orchestror.getComponent<Position>(e);
        if (!lastEntityPositions.count(e) || lastEntityPositions[e].x != pos.x || lastEntityPositions[e].y != pos.y) {
            EntityData data{e, pos.x, pos.y};
            deltaEntities.push_back(data);
            lastEntityPositions[e] = pos;
        }
    }

    if (!deltaEntities.empty()) {
        SnapshotPacket snap{};
        snap.header.type = PacketType::SNAPSHOT;
        snap.header.size = sizeof(SnapshotPacket);
        snap.tick = tick++;
        size_t count = std::min(deltaEntities.size(), static_cast<size_t>(M_ENTITIES));
        snap.num_entities = static_cast<uint16_t>(count);
        std::memcpy(snap.entities, deltaEntities.data(), sizeof(EntityData) * count);

        NetworkManager::getInstance().sendSnapshot(snap);
    }
}

void GameServer::onEnemyKilled(uint32_t killerPlayerId) {
    auto& stats = getPlayerStats(killerPlayerId);
    stats.enemiesKilled++;
    stats.score += 100;
}

void GameServer::onPlayerHit(uint32_t playerId) {
    auto& stats = getPlayerStats(playerId);
    stats.lives--;
}

PlayerGameStats& GameServer::getPlayerStats(uint32_t playerId) {
    if (!playerStats.count(playerId))
        playerStats[playerId] = PlayerGameStats();
    return playerStats[playerId];
}

std::vector<EntityData> GameServer::getEntitiesState() const {
    std::vector<EntityData> entities;
    for (auto e : orchestror.getEntities()) {
        if (!orchestror.hasComponent<Position>(e)) continue;
        auto& pos = orchestror.getComponent<Position>(e);
        entities.push_back({e, pos.x, pos.y});
    }
    return entities;
}