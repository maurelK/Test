/*
** EPITECH PROJECT, 2025
** r"
** File description:
** efe
*/

#pragma once
#include "EntityManager.hpp"
#include "ComponentStorage.hpp"

class SpawnSystem {
public:
    EntityID spawnEnemy(EntityManager& em, ComponentStorage& storage);
};
