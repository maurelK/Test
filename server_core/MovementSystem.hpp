/*
** EPITECH PROJECT, 2025
** dz
** File description:
** zffz
*/

#pragma once

#include "ComponentStorage.hpp"
#include <vector>
#include <cstdint>

struct PlayerInput {
    EntityID playerId;
    float dx = 0.f;
    float dy = 0.f;
};

class MovementSystem {
public:
    void update(float dt, const std::vector<PlayerInput>& inputs, ComponentStorage& storage);
};
