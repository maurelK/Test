/*
** EPITECH PROJECT, 2025
** ef
** File description:
** fe
*/

#include "HealthSystem.hpp"
#include <iostream>

void HealthSystem::update(float)
{
    for (auto e : entities) {
        if (healths.hasData(e)) {
            auto& hp = healths.getData(e);
            if (hp.hp <= 0) {
                std::cout << "Entity " << e << " died\n";
                entityManager.destroyEntity(e);
            }
        }
    }
}
