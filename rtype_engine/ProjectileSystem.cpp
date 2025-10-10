/*
** EPITECH PROJECT, 2025
** re
** File description:
** et
*/

#include "ProjectileSystem.hpp"
#include <iostream>

Entity ProjectileSystem::spawnProjectile(Entity owner, float x, float y, float dx, float dy, int damage, float life) {
    Entity p = entityManager.createEntity();
    positions.insertData(p, {x, y});
    velocities.insertData(p, {dx, dy});
    projectiles.insertData(p, {owner, damage, life});

    addEntity(p);
    std::cout << "[ProjectileSystem] Projectile spawned at (" << x << "," << y << ")\n";
    return p;
}

void ProjectileSystem::update(float dt) {
    std::vector<Entity> toRemove;

    for (auto e : entities) {
        if (!positions.hasData(e) || !velocities.hasData(e) || !projectiles.hasData(e))
            continue;

        auto& pos = positions.getData(e);
        auto& vel = velocities.getData(e);
        auto& proj = projectiles.getData(e);

        pos.x += vel.dx * dt;
        pos.y += vel.dy * dt;

        proj.life -= dt;
        if (proj.life <= 0) {
            toRemove.push_back(e);
            continue;
        }
        for (auto target : entityManager.getLivingEntities()) {
            if (target == proj.owner) continue;
            if (!positions.hasData(target) || !healths.hasData(target)) continue;

            auto& targetPos = positions.getData(target);
            auto& targetHealth = healths.getData(target);

            float dx = pos.x - targetPos.x;
            float dy = pos.y - targetPos.y;
            float dist2 = dx*dx + dy*dy;
            const float collisionRadius2 = 4.0f;

            if (dist2 <= collisionRadius2) {
                targetHealth.hp -= proj.damage;
                toRemove.push_back(e);
                std::cout << "[ProjectileSystem] Projectile " << e << " hit Entity " << target
                          << " for " << proj.damage << " damage\n";
                break;
            }
        }
    }

    for (auto e : toRemove) {
        positions.entityDestroyed(e);
        velocities.entityDestroyed(e);
        projectiles.entityDestroyed(e);
        entityManager.destroyEntity(e);
        removeEntity(e);
    }
}
