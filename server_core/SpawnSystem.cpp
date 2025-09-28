/*
** EPITECH PROJECT, 2025
** fef
** File description:
** fefe
*/

#include "SpawnSystem.hpp"

EntityID SpawnSystem::spawnEnemy(EntityManager& em, ComponentStorage& storage) {
    EntityID id = em.createEntity();
    storage.addPosition(id, Position{100, 50});
    storage.addVelocity(id, Velocity{-80, 0});
    storage.addHealth(id, Health{100});
    return id;
}
