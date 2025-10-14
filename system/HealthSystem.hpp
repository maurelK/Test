/*
** EPITECH PROJECT, 2025
** ef
** File description:
** fe
*/

/*
** EPITECH PROJECT, 2025
** R-Type HealthSystem Header
*/

#pragma once
#include "../rtype_engine/System.hpp"
#include "../rtype_engine/Components.hpp"
#include "../rtype_engine/Component_storage.hpp"
#include "../rtype_engine/EntityManager.hpp"

class HealthSystem : public System {
public:
    HealthSystem(ComponentArray<Health>& h, EntityManager& em)
        : healths(h), entityManager(em) {}

    void update(float dt) override;

private:
    ComponentArray<Health>& healths;
    EntityManager& entityManager;
};
