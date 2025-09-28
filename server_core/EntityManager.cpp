/*
** EPITECH PROJECT, 2025
** entity
** File description:
** rtype
*/

#include "EntityManager.hpp"
#include <algorithm>

EntityID EntityManager::createEntity() {
    EntityID id = entities.empty() ? 0 : entities.back() + 1;
    entities.push_back(id);
    return id;
}

void EntityManager::destroyEntity(EntityID id) {
    entities.erase(std::remove(entities.begin(), entities.end(), id), entities.end());
}

