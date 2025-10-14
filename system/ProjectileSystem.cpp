/*
** EPITECH PROJECT, 2025
** re
** File description:
** et
*/

#include "ProjectileSystem.hpp"
#include <iostream>

Entity ProjectileSystem::spawnProjectile(Entity owner, float x, float y, float dx, float dy, int damage, float life)
{
    Entity e = entityManager.createEntity();
    positions.insertData(e, {x, y});
    velocities.insertData(e, {dx, dy});
    projectiles.insertData(e, {owner, damage, life});
    return e;
}

void ProjectileSystem::update(float dt)
{
    std::vector<Entity> toRemove;
    for (auto e : projectiles.getAllEntities()) {
        if (!positions.hasData(e)) continue;
        auto& pos = positions.getData(e);
        auto& vel = velocities.getData(e);
        pos.x += vel.dx * dt;
        pos.y += vel.dy * dt;

        auto& proj = projectiles.getData(e);
        proj.life -= dt;
        if (proj.life <= 0) {
            toRemove.push_back(e);
        }
    }

    for (auto e : toRemove) {
        entityManager.destroyEntity(e);
    }
}
