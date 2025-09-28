/*
** EPITECH PROJECT, 2025
** ee
** File description:
** fefe
*/

#pragma once
#include "ComponentStorage.hpp"

class HealthSystem {
public:
    bool applyDamage(ComponentStorage& storage, EntityID id, int dmg);
};
