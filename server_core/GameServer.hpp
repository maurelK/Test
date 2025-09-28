/*
** EPITECH PROJECT, 2025
** ffe
** File description:
** feef
*/

#pragma once
#include "ComponentStorage.hpp"
#include "EntityManager.hpp"
#include "SpawnSystem.hpp"
#include "MovementSystem.hpp"
#include "Snapshot.hpp"

class GameServer {
private:
    ComponentStorage storage;
    EntityManager em;
    SpawnSystem spawn;

    Snapshot generateSnapshot() const;

public:
    GameServer() = default;

    void spawnEnemy() { spawn.spawnEnemy(em, storage); }
    Snapshot getSnapshot() const { return generateSnapshot(); }
    const std::vector<EntityID>& getEntityIDs() const { return em.getEntities(); }
};
