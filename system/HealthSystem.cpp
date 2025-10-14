/*
** EPITECH PROJECT, 2025
** ef
** File description:
** fe
*/

#include "HealthSystem.hpp"

void HealthSystem::update(float dt) {
    for (auto e : entities) {
        if (healths.hasData(e)) {
            auto& h = healths.getData(e);
            if (h.hp > 0) h.hp--;
        }
    }
}
