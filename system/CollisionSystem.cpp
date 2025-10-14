/*
** EPITECH PROJECT, 2025
** ef
** File description:
** fe
*/

#include "CollisionSystem.hpp"

void CollisionSystem::update(float dt) {
    for (auto e1 : entities) {
        for (auto e2 : entities) {
            if (e1 == e2) continue;
            if (!positions.hasData(e1) || !positions.hasData(e2)) continue;

            auto& p1 = positions.getData(e1);
            auto& p2 = positions.getData(e2);

            if (p1.x == p2.x && p1.y == p2.y) {
                if (healths.hasData(e1)) healths.getData(e1).hp--;
                if (healths.hasData(e2)) healths.getData(e2).hp--;
            }
        }
    }
}
