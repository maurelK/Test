/*
** EPITECH PROJECT, 2025
** re
** File description:
** et
*/

#include "ProjectileSystem.hpp"
#include "../engine/include/ECS_architecture/Components.hpp"
#include <iostream>

ProjectileSystem::ProjectileSystem(Orchestror& orchestror)
: orchestror(orchestror) {}

// Création du projectile
Entity ProjectileSystem::spawnProjectile(Entity owner, float x, float y, float dx, float dy, int damage, float life)
{
    Entity p = orchestror.createEntity();
    orchestror.addComponent(p, Position{x, y});
    orchestror.addComponent(p, Velocity{dx, dy});
    orchestror.addComponent(p, Projectile{owner, damage, life});
    return p;
}

// Mise à jour des projectiles
void ProjectileSystem::update(float dt)
{
    for (auto e : orchestror.getEntities()) {
        if (!orchestror.hasComponent<Position>(e) ||
            !orchestror.hasComponent<Velocity>(e) ||
            !orchestror.hasComponent<Projectile>(e))
            continue;

        auto& pos = orchestror.getComponent<Position>(e);
        auto& vel = orchestror.getComponent<Velocity>(e);
        auto& proj = orchestror.getComponent<Projectile>(e);

        pos.x += vel.dx * dt;
        pos.y += vel.dy * dt;

        proj.life -= dt;
        if (proj.life <= 0.f && onKillCallback)
            onKillCallback(proj.owner, e);
    }
}