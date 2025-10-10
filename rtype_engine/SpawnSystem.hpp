/*
** EPITECH PROJECT, 2025
** fefe
** File description:
** fe
*/

#ifndef SPAWN_SYSTEM_HPP
#define SPAWN_SYSTEM_HPP

#include "System.hpp"
#include "Component_storage.hpp"
#include "EntityManager.hpp"
#include "Components.hpp"


class SpawnSystem : public System {
private:
    EntityManager& entityManager;
    ComponentStorage<Position>& positions;
    ComponentStorage<Velocity>& velocities;
    ComponentStorage<Health>& healths;

public:
    SpawnSystem(EntityManager& em, ComponentStorage<Position>& p,
                ComponentStorage<Velocity>& v, ComponentStorage<Health>& h)
        : entityManager(em), positions(p), velocities(v), healths(h) {}

    void update(float dt) override;
    Entity spawnEnemy(float x, float y);
};

#endif
