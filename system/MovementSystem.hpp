/*
** EPITECH PROJECT, 2025
** effe
** File description:
** fefe
*/

#ifndef MOVEMENT_SYSTEM_HPP
#define MOVEMENT_SYSTEM_HPP

#include "../rtype_engine/System.hpp"
#include "../rtype_engine/Component_storage.hpp"
#include "../rtype_engine/Components.hpp"


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
