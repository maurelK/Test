/*
** EPITECH PROJECT, 2025
** effe
** File description:
** fefe
*/

#pragma once
//#include "../engine/include/ECS_architecture/System.hpp"
#include "../engine/include/ECS_architecture/Orchestror.hpp"
#include "../engine/include/ECS_architecture/Components.hpp"


class MovementSystem : public System {
private:
    Orchestror& orchestror;

public:
    MovementSystem(Orchestror& orch) : orchestror(orch) {}
    void update(float dt) override;
};
