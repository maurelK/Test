/*
** EPITECH PROJECT, 2025
** ef
** File description:
** fe
*/

#include "SpawnSystem.hpp"

void SpawnSystem::update(float dt)
{
    // SpawnSystem doesn't need to update every frame
    // Spawning is done on-demand via spawnEnemy()
}

Entity SpawnSystem::spawnEnemy(float x, float y)
{
    Entity e = entityManager.createEntity();
    positions.insertData(e, {x, y});
    velocities.insertData(e, {0.f, 0.f});
    healths.insertData(e, {100});
    return e;
}

