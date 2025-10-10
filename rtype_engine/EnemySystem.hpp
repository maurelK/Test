/*
** EPITECH PROJECT, 2025
** ef
** File description:
** ef
*/


#ifndef ENEMYSYSTEM_HPP
#define ENEMYSYSTEM_HPP

#include "EntityManager.hpp"
#include "Component_storage.hpp"
#include "Components.hpp"
#include <random>

class EnemySystem {
public:
    EnemySystem(EntityManager& em,
                ComponentStorage<Position>& pos,
                ComponentStorage<Velocity>& vel,
                ComponentStorage<Health>& hp);

    Entity spawnEnemy(float x, float y);
    void update(float dt);

private:
    EntityManager& entityManager;
    ComponentStorage<Position>& positions;
    ComponentStorage<Velocity>& velocities;
    ComponentStorage<Health>& healths;

    std::mt19937 gen;
    std::uniform_real_distribution<float> moveDist;
};

#endif
