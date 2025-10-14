/*
** EPITECH PROJECT, 2025
** fefe
** File description:
** fe
*/

#pragma once
#include "../rtype_engine/System.hpp"
#include "../rtype_engine/Components.hpp"
#include "../rtype_engine/Component_storage.hpp"
#include "../rtype_engine/EntityManager.hpp"

class SpawnSystem : public System {
public:
    SpawnSystem(EntityManager& em,
                ComponentArray<Position>& pos,
                ComponentArray<Velocity>& vel,
                ComponentArray<Health>& hp)
        : entityManager(em), positions(pos), velocities(vel), healths(hp) {}

    void update(float dt) override;
    Entity spawnEnemy(float x, float y);

private:
    EntityManager& entityManager;
    ComponentArray<Position>& positions;
    ComponentArray<Velocity>& velocities;
    ComponentArray<Health>& healths;
};
