/*
** EPITECH PROJECT, 2025
** fezef
** File description:
** effe
*/

#include "HealthSystem.hpp"

bool HealthSystem::applyDamage(ComponentStorage& storage, EntityID id, int dmg) {
    if (Health* h = storage.getHealth(id)) {
        h->hp -= dmg;
        return h->hp <= 0;
    }
    return false;
}
