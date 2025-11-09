/*
** EPITECH PROJECT, 2025
** ef
** File description:
** fe
*/

#include "SpawnSystem.hpp"

Entity SpawnSystem::spawnEnemy(float x, float y) {
    Entity e = orchestror.createEntity();
    orchestror.addComponent<Position>(e, Position{x, y});
    orchestror.addComponent<Velocity>(e, Velocity{0.f, 0.f});
    orchestror.addComponent<Health>(e, Health{100});
    return e;
}
