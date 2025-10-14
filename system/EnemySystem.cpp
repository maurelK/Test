/*
** EPITECH PROJECT, 2025
** d
** File description:
** d
*/

#include "EnemySystem.hpp"
#include <iostream>

EnemySystem::EnemySystem(EntityManager& em,
                         ComponentStorage<Position>& pos,
                         ComponentStorage<Velocity>& vel,
                         ComponentStorage<Health>& hp)
    : entityManager(em), positions(pos), velocities(vel), healths(hp),
      gen(std::random_device{}()), moveDist(-50.f, 50.f)
{}

Entity EnemySystem::spawnEnemy(float x, float y) {
    Entity e = entityManager.createEntity();
    positions.insertData(e, {x, y});
    velocities.insertData(e, {moveDist(gen), moveDist(gen)});
    healths.insertData(e, {100});
    std::cout << "[EnemySystem] Enemy spawned at (" << x << ", " << y << ")\n";
    return e;
}

void EnemySystem::update(float dt) {
    for (auto e : entityManager.getLivingEntities()) {
       if (positions.hasData(e) && velocities.hasData(e)) {
            auto& pos = positions.getData(e);
            auto& vel = velocities.getData(e);
            pos.x += vel.dx * dt;
            pos.y += vel.dy * dt;
        }
    }
}
