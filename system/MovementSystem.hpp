/*
** EPITECH PROJECT, 2025
** effe
** File description:
** fefe
*/


#pragma once
#include "../rtype_engine/System.hpp"
#include "../rtype_engine/Components.hpp"
#include "../rtype_engine/Component_storage.hpp"

class MovementSystem : public System {
public:
    MovementSystem(ComponentArray<Position>& pos, ComponentArray<Velocity>& vel)
        : positions(pos), velocities(vel) {}

    void update(float dt) override;

private:
    ComponentArray<Position>& positions;
    ComponentArray<Velocity>& velocities;
};
