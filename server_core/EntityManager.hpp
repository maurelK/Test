/*
** EPITECH PROJECT, 2025
** efe
** File description:
** feef
*/

#pragma once
#include <vector>
#include <cstdint>
#include "ComponentStorage.hpp"

using EntityID = uint32_t;
//inline constexpr EntityID INVALID_ENTITY = UINT32_MAX;

class EntityManager {
private:
    std::vector<EntityID> entities;

public:
    EntityID createEntity();
    void destroyEntity(EntityID id);
    const std::vector<EntityID>& getEntities() const { return entities; }
};
