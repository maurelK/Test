/*
** EPITECH PROJECT, 2025
** dz
** File description:
** zffz
*/

#pragma once
#include "ComponentStorage.hpp"
#include <vector>

struct PlayerInput { float dx, dy; }; // simplifié

class MovementSystem {
public:
    void update(float deltaTime, const std::vector<PlayerInput>& inputs, ComponentStorage& storage);
};
