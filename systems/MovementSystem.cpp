/*
** EPITECH PROJECT, 2025
** edff
** File description:
** effe
*/

#include "MovementSystem.hpp"

void MovementSystem::update(float deltaTime, const std::vector<PlayerInput>& inputs, ComponentStorage& storage) {
    size_t i = 0;
    for (auto& [id, pos] : storage.getPositions()) {
        if (i < inputs.size()) {
            pos.x += inputs[i].dx * deltaTime;
            pos.y += inputs[i].dy * deltaTime;
        }
        i++;
    }
}
