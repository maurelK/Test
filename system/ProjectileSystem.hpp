/*
** EPITECH PROJECT, 2025
** ef
** File description:
** f
*/

#pragma once
#include "../engine/include/ECS_architecture/Orchestror.hpp"
#include "../engine/include/ECS_architecture/Components.hpp"
//#include "System.hpp"
#include <functional>

class ProjectileSystem : public System {
public:
    ProjectileSystem(Orchestror& orchestror);

    Entity spawnProjectile(Entity owner, float x, float y, float dx, float dy, int damage, float life);
    void update(float dt) override;

    void setOnKillCallback(std::function<void(Entity, Entity)> cb) { onKillCallback = cb; }

private:
    Orchestror& orchestror;
    std::function<void(Entity, Entity)> onKillCallback;
};