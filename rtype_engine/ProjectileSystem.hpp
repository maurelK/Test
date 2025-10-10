/*
** EPITECH PROJECT, 2025
** ef
** File description:
** f
*/

#ifndef PROJECTILE_SYSTEM_HPP
#define PROJECTILE_SYSTEM_HPP

#include "EntityManager.hpp"
#include "Component_storage.hpp"
#include "Components.hpp"
#include "System.hpp"
#include <vector>

class ProjectileSystem : public System {
private:
    EntityManager& entityManager;
    ComponentStorage<Position>& positions;
    ComponentStorage<Velocity>& velocities;
    ComponentStorage<Projectile>& projectiles;
    ComponentStorage<Health>& healths;

public:
    ProjectileSystem(EntityManager& em,
                     ComponentStorage<Position>& pos,
                     ComponentStorage<Velocity>& vel,
                     ComponentStorage<Projectile>& proj,
                     ComponentStorage<Health>& hp)
        : entityManager(em), positions(pos), velocities(vel),
          projectiles(proj), healths(hp) {}

    Entity spawnProjectile(Entity owner, float x, float y, float dx, float dy, int damage = 10, float life = 5.0f);

    void update(float dt) override;
};

#endif
