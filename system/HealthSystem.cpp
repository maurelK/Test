/*
** EPITECH PROJECT, 2025
** ef
** File description:
** fe
*/

#include "HealthSystem.hpp"
#include <iostream>

void HealthSystem::update(float dt) {
    (void)dt;
    for (auto e : entities) {
        if (orchestror.hasComponent<Health>(e)) {
            auto& health = orchestror.getComponent<Health>(e);
            if (health.hp <= 0) {
                std::cout << "Entity " << e << " est morte.\n";
                orchestror.destroyEntity(e);
            }
        }
    }
}
