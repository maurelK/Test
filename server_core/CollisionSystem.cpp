/*
** EPITECH PROJECT, 2025
** fe
** File description:
** fz
*/

#include "CollisionSystem.hpp"

void CollisionSystem::update(const ComponentStorage& storage, CollisionHandler handler) {
    for (auto it1 = storage.getAllPositions().begin(); it1 != storage.getAllPositions().end(); ++it1) {
        for (auto it2 = std::next(it1); it2 != storage.getAllPositions().end(); ++it2) {
            if (it1->second.x == it2->second.x && it1->second.y == it2->second.y) {
                handler(it1->first, it2->first);
            }
        }
    }
}
