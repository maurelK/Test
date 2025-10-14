/*
** EPITECH PROJECT, 2025
** ff
** File description:
** fefe
*/

#include "MovementSystem.hpp"

void MovementSystem::update(float dt) {
    for (auto e : entities) {
        if (positions.hasData(e) && velocities.hasData(e)) {
            auto& pos = positions.getData(e);
            auto& vel = velocities.getData(e);
            pos.x += vel.dx * dt;
            pos.y += vel.dy * dt;
        }
    }
}
