/*
** EPITECH PROJECT, 2025
** ef
** File description:
** fe
*/

#include "SpawnSystem.hpp"

void SpawnSystem::update(float dt) {
    // Pas encore de logique automatique de spawn ici
}

Entity SpawnSystem::spawnEnemy(float x, float y) {
    Entity e = entityManager.createEntity();
    positions.insertData(e, Position{x, y});
    velocities.insertData(e, Velocity{-80, 0});
    healths.insertData(e, Health{100});
    return e;
}
