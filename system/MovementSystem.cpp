/*
** EPITECH PROJECT, 2025
** ff
** File description:
** fefe
*/

#include "MovementSystem.hpp"

void MovementSystem::update(float dt) {
    (void)dt;
    for (auto e : entities) {
        if (orchestror.hasComponent<Position>(e) && orchestror.hasComponent<Velocity>(e)) {
            auto& pos = orchestror.getComponent<Position>(e);
            auto& vel = orchestror.getComponent<Velocity>(e);
            pos.x += vel.dx * dt;
            pos.y += vel.dy * dt;
        }
    }
}
