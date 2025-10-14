/*
** EPITECH PROJECT, 2025
** ef
** File description:
** fe
*/

#include "CollisionSystem.hpp"
#include <iostream>
#include <cmath>

void CollisionSystem::update(float)
{
    auto list = positions.getAllEntities();
    for (auto a : list) {
        for (auto b : list) {
            if (a == b) continue;
            auto& pa = positions.getData(a);
            auto& pb = positions.getData(b);
            float dist = std::sqrt((pa.x - pb.x)*(pa.x - pb.x) + (pa.y - pb.y)*(pa.y - pb.y));
            if (dist < 10.f && healths.hasData(a) && healths.hasData(b)) {
                healths.getData(a).hp -= 1;
                healths.getData(b).hp -= 1;
            }
        }
    }
}
