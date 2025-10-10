/*
** EPITECH PROJECT, 2025
** effe
** File description:
** fefe
*/

#ifndef MOVEMENT_SYSTEM_HPP
#define MOVEMENT_SYSTEM_HPP

#include "System.hpp"
#include "Component_storage.hpp"
#include "Components.hpp"


class MovementSystem : public System {
private:
    ComponentStorage<Position>& positions;
    ComponentStorage<Velocity>& velocities;

public:
    MovementSystem(ComponentStorage<Position>& p, ComponentStorage<Velocity>& v)
        : positions(p), velocities(v) {}

    void update(float dt) override;
};

#endif
