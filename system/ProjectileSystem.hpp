/*
** EPITECH PROJECT, 2025
** ef
** File description:
** f
*/

#pragma once
#include "../rtype_engine/System.hpp"
#include "../rtype_engine/Components.hpp"
#include "../rtype_engine/Component_storage.hpp"
#include "../rtype_engine/EntityManager.hpp"

class ProjectileSystem : public System {
public:
    ProjectileSystem(
        EntityManager& em,
        ComponentArray<Position>& pos,
        ComponentArray<Velocity>& vel,
        ComponentArray<Projectile>& proj
    ) : entityManager(em), positions(pos), velocities(vel), projectiles(proj) {}

    void update(float dt) override;
    Entity spawnProjectile(Entity owner, float x, float y, float dx, float dy, int damage = 10, float life = 5.0f);

private:
    EntityManager& entityManager;
    ComponentArray<Position>& positions;
    ComponentArray<Velocity>& velocities;
    ComponentArray<Projectile>& projectiles;
};
