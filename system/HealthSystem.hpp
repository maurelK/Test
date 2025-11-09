/*
** EPITECH PROJECT, 2025
** ef
** File description:
** fe
*/

#pragma once
//#include "../engine/include/ECS_architecture/System.hpp"
#include "../engine/include/ECS_architecture/Orchestror.hpp"
#include "../engine/include/ECS_architecture/Components.hpp"


class HealthSystem : public System {
private:
    Orchestror& orchestror;

public:
    HealthSystem(Orchestror& orch) : orchestror(orch) {}
    void update(float dt) override;
};
