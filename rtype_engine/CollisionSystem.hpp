/*
** EPITECH PROJECT, 2025
** fe
** File description:
** fe
*/

#ifndef COLLISION_SYSTEM_HPP
#define COLLISION_SYSTEM_HPP

#include "System.hpp"
#include "Component_storage.hpp"
#include "Components.hpp"


class CollisionSystem : public System {
private:
    ComponentStorage<Position>& positions;
    ComponentStorage<Health>& healths;

public:
    CollisionSystem(ComponentStorage<Position>& p, ComponentStorage<Health>& h)
        : positions(p), healths(h) {}

    void update(float dt) override;
};

#endif
