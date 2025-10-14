/*
** EPITECH PROJECT, 2025
** fe
** File description:
** fe
*/

/*
** EPITECH PROJECT, 2025
** R-Type CollisionSystem Header
*/

#pragma once
#include "../rtype_engine/System.hpp"
#include "../rtype_engine/Components.hpp"
#include "../rtype_engine/Component_storage.hpp"

class CollisionSystem : public System {
public:
    CollisionSystem(ComponentArray<Position>& pos, ComponentArray<Health>& hp)
        : positions(pos), healths(hp) {}

    void update(float dt) override;

private:
    ComponentArray<Position>& positions;
    ComponentArray<Health>& healths;
};
