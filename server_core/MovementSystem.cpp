/*
** EPITECH PROJECT, 2025
** edff
** File description:
** effe
*/

/*
** EPITECH PROJECT, 2025
** MovementSystem
** File description:
** Implementation of MovementSystem
*/

#include "MovementSystem.hpp"

void MovementSystem::update(float dt, const std::vector<PlayerInput>& inputs, ComponentStorage& storage) {
    for (const auto& input : inputs) {
        EntityID id = input.playerId;

        Position* pos = storage.getPosition(id);
        Velocity* vel = storage.getVelocity(id);

        if (pos && vel) {
            pos->x += vel->dx * dt;
            pos->y += vel->dy * dt;
            pos->x += input.dx * dt;
            pos->y += input.dy * dt;
        }
    }
}

