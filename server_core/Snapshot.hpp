/*
** EPITECH PROJECT, 2025
** ef
** File description:
** fefe
*/

#pragma once
#include "ComponentStorage.hpp"
#include <vector>

struct Snapshot {
    struct EntityData {
        EntityID id;
        Position pos;
        Velocity vel;
        Health hp;
    };

    std::vector<EntityData> entities;
};

