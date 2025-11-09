/*
** EPITECH PROJECT, 2025
** fefe
** File description:
** fe
*/

#pragma once
//#include "../engine/include/ECS_architecture/System.hpp"
#include "../engine/include/ECS_architecture/Orchestror.hpp"
#include "../engine/include/ECS_architecture/Components.hpp"


class SpawnSystem : public System {
private:
    Orchestror& orchestror;

public:
    SpawnSystem(Orchestror& orch) : orchestror(orch) {}
    void update(float dt) override { (void)dt; }

    Entity spawnEnemy(float x, float y);
};
