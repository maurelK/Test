/*
** EPITECH PROJECT, 2025
** ef
** File description:
** ef
*/


#pragma once
//#include "../engine/include/ECS_architecture/System.hpp"
#include "../engine/include/ECS_architecture/Orchestror.hpp"
#include "../engine/include/ECS_architecture/Components.hpp"

class EnemySystem : public System {
private:
    Orchestror& orchestror;

public:
    EnemySystem(Orchestror& orch) : orchestror(orch) {}
    void update(float dt) override;
};

