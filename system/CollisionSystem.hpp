/*
** EPITECH PROJECT, 2025
** fe
** File description:
** fe
*/

#ifndef COLLISION_SYSTEM_HPP
#define COLLISION_SYSTEM_HPP

#include "../rtype_engine/System.hpp"
#include "../rtype_engine/Component_storage.hpp"
#include "../rtype_engine/Components.hpp"


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
