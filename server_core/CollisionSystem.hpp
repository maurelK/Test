/*
** EPITECH PROJECT, 2025
** feef
** File description:
** fefe
*/

#pragma once
#include "ComponentStorage.hpp"
#include <functional>

using CollisionHandler = std::function<void(EntityID, EntityID)>;

class CollisionSystem {
public:
    void update(const ComponentStorage& storage, CollisionHandler handler);
};

