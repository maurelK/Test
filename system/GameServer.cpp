#include "GameServer.hpp"
#include <iostream>
#include <thread>

GameServer::GameServer() {
    orchestr = std::make_unique<Orchestror>();
}

void GameServer::initialize() {
    // Initialiser l'Orchestror
    orchestr->init();
    
    // Enregistrer tous les composants via Orchestror
    orchestr->registerComponent<Position>();
    orchestr->registerComponent<Velocity>();
    orchestr->registerComponent<Health>();
    orchestr->registerComponent<Projectile>();
    orchestr->registerComponent<Player>(); // Pour identifier les joueurs

    // Créer les systèmes via Orchestror
    moveSys = orchestr->registerSystem<MovementSystem>(*orchestr);
    healthSys = orchestr->registerSystem<HealthSystem>(*orchestr);
    spawnSys = orchestr->registerSystem<SpawnSystem>(*orchestr);
    collisionSys = orchestr->registerSystem<CollisionSystem>(*orchestr);
    projectileSys = orchestr->registerSystem<ProjectileSystem>(*orchestr);

    // Définir les signatures des systèmes
    Signature movementSig;
    movementSig.set(orchestr->getComponentType<Position>());
    movementSig.set(orchestr->getComponentType<Velocity>());
    orchestr->setSystemSignature<MovementSystem>(movementSig);

    Signature healthSig;
    healthSig.set(orchestr->getComponentType<Health>());
    orchestr->setSystemSignature<HealthSystem>(healthSig);

    Signature collisionSig;
    collisionSig.set(orchestr->getComponentType<Position>());
    collisionSig.set(orchestr->getComponentType<Health>());
    orchestr->setSystemSignature<CollisionSystem>(collisionSig);

    Signature projectileSig;
    projectileSig.set(orchestr->getComponentType<Position>());
    projectileSig.set(orchestr->getComponentType<Velocity>());
    projectileSig.set(orchestr->getComponentType<Projectile>());
    orchestr->setSystemSignature<ProjectileSystem>(projectileSig);

    // Créer des entités de test
    Entity e1 = spawnEnemy(100.f, 50.f);
    Entity e2 = spawnEnemy(200.f, 60.f);

    std::cout << "[GameServer] Initialisé avec " << orchestr->getEntityCount() << " entités\n";
}

void GameServer::update(float dt) {
    // Traiter les inputs réseau
    InputPacket input;
    while(NetworkManager::getInstance().popInput(input)) {
        handleInput(input.player_id, input.move_x, input.move_y, input.shoot);
    }
    
    // Mettre à jour tous les systèmes via Orchestror
    orchestr->update(dt);
    
    // Envoyer les snapshots
    sendSnapshots();
}

Entity GameServer::spawnEnemy(float x, float y) {
    Entity enemy = orchestr->createEntity();
    orchestr->addComponent(enemy, Position{x, y});
    orchestr->addComponent(enemy, Velocity{-80.f, 0.f});
    orchestr->addComponent(enemy, Health{100});
    
    std::cout << "[GameServer] Ennemi créé: " << enemy << " à (" << x << ", " << y << ")\n";
    return enemy;
}

Entity GameServer::spawnProjectileFrom(Entity owner, float dx, float dy, int damage, float life) {
    if (!orchestr->hasComponent<Position>(owner)) {
        return static_cast<Entity>(-1);
    }

    // Récupérer la position du propriétaire
    auto& ownerPos = orchestr->getComponent<Position>(owner);
    
    // Créer le projectile
    Entity projectile = orchestr->createEntity();
    orchestr->addComponent(projectile, Position{ownerPos.x, ownerPos.y});
    orchestr->addComponent(projectile, Velocity{dx, dy});
    orchestr->addComponent(projectile, Projectile{true, owner, damage, life});
    
    std::cout << "[GameServer] Projectile créé: " << projectile 
              << " par " << owner << " à (" << ownerPos.x << ", " << ownerPos.y << ")\n";
    return projectile;
}

void GameServer::handleInput(uint32_t playerId, float moveX, float moveY, bool shoot) {
    // Vérifier si le joueur existe
    bool playerExists = false;
    for (Entity entity : orchestr->getLivingEntities()) {
        if (orchestr->hasComponent<Player>(entity) && 
            orchestr->getComponent<Player>(entity).id == static_cast<int>(playerId)) {
            playerExists = true;
            
            // Mettre à jour la vélocité
            if (orchestr->hasComponent<Velocity>(entity)) {
                auto& vel = orchestr->getComponent<Velocity>(entity);
                vel.dx = moveX * 100.f;
                vel.dy = moveY * 100.f;
            }
            
            // Gérer le tir
            if (shoot) {
                spawnProjectileFrom(entity, 200.f, 0.f);
            }
            break;
        }
    }
    
    // Créer un nouveau joueur si nécessaire
    if (!playerExists) {
        Entity newPlayer = orchestr->createEntity();
        orchestr->addComponent(newPlayer, Position{50.f + playerId * 30, 50.f});
        orchestr->addComponent(newPlayer, Velocity{0.f, 0.f});
        orchestr->addComponent(newPlayer, Health{100});
        orchestr->addComponent(newPlayer, Player{static_cast<int>(playerId)});
        
        std::cout << "[GameServer] Nouveau joueur créé: ID " << playerId 
                  << " (entité " << newPlayer << ")\n";
    }
}

std::vector<EntityData> GameServer::getEntitiesState() const {
    std::vector<EntityData> entities;
    
    for (Entity entity : orchestr->getLivingEntities()) {
        if (orchestr->hasComponent<Position>(entity)) {
            auto& pos = orchestr->getComponent<Position>(entity);
            entities.push_back({entity, pos.x, pos.y});
        }
    }
    
    return entities;
}

void GameServer::sendSnapshots() {
    auto entities = getEntitiesState();
    
    if (entities.empty()) return;

    SnapshotPacket snap;
    snap.header.type = PacketType::SNAPSHOT;
    snap.header.size = sizeof(SnapshotPacket);
    snap.tick = tick++;
    snap.num_entities = std::min(entities.size(), static_cast<size_t>(M_ENTITIES));
    
    // Copier les données d'entités
    for (size_t i = 0; i < snap.num_entities; ++i) {
        snap.entities[i] = entities[i];
    }
    
    // Envoyer le snapshot
    NetworkManager::getInstance().sendSnapshot(snap);
    
    std::cout << "[GameServer] Snapshot envoyé - tick: " << snap.tick 
              << ", entités: " << snap.num_entities << "\n";
}