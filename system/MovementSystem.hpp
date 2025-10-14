#ifndef MOVEMENT_SYSTEM_HPP
#define MOVEMENT_SYSTEM_HPP

#include "../rtype_engine/System.hpp"
#include "../rtype_engine/Orchestror.hpp"

class MovementSystem : public System {
private:
    Orchestror& orchestr;

public:
    MovementSystem(Orchestror& o) : orchestr(o) {}

    void update(float dt) override {
        for (auto entity : entities) {
            if (orchestr.hasComponent<Position>(entity) && 
                orchestr.hasComponent<Velocity>(entity)) {
                
                auto& pos = orchestr.getComponent<Position>(entity);
                auto& vel = orchestr.getComponent<Velocity>(entity);
                
                pos.x += vel.dx * dt;
                pos.y += vel.dy * dt;
            }
        }
    }
};

#endif