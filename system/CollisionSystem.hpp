#pragma once
//#include "../engine/include/ECS_architecture/System.hpp"
#include "../engine/include/ECS_architecture/Orchestror.hpp"
#include "../engine/include/ECS_architecture/Components.hpp"


class CollisionSystem : public System {
private:
    Orchestror& orchestror;

public:
    CollisionSystem(Orchestror& orch) : orchestror(orch) {}

    void update(float dt) override;
};
