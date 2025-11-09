/*
** EPITECH PROJECT, 2025
** d
** File description:
** d
*/

#include "EnemySystem.hpp"
#include <iostream>


#include "EnemySystem.hpp"

void EnemySystem::update(float dt) {
    (void)dt;
    for (auto e : entities) {
        if (orchestror.hasComponent<Velocity>(e)) {
            auto& vel = orchestror.getComponent<Velocity>(e);
            vel.dx = -50.f; // simple move left
        }
    }
}

