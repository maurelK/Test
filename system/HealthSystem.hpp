/*
** EPITECH PROJECT, 2025
** ef
** File description:
** fe
*/

#ifndef HEALTH_SYSTEM_HPP
#define HEALTH_SYSTEM_HPP

#include "../rtype_engine/System.hpp"
#include "../rtype_engine/Component_storage.hpp"
#include "../rtype_engine/Components.hpp"

class HealthSystem : public System {
private:
    ComponentStorage<Health>& healths;

public:
    HealthSystem(ComponentStorage<Health>& h) : healths(h) {}
    void update(float dt) override;
};

#endif
