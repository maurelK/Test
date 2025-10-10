#pragma once
#include "System.hpp"
#include "Orchestror.hpp"
#include "../rtype_engine/Components.hpp"
#include <iostream>


class MovementSystem : public System {
    public:
        MovementSystem(Orchestror& o) : orchestror(o) {}
        
        void update(float dt) override {
            for(auto entity : entities) {
                if(orchestror.hasComponent<Position>(entity) &&
                    orchestror.hasComponent<Velocity>(entity)) {
                    
                    auto& pos = orchestror.getComponent<Position>(entity);
                    auto& vel  = orchestror.getComponent<Velocity>(entity);

                    pos.x += vel.dx * dt;
                    pos.y += vel.dy * dt;

                    std::cout << "[Mouvement] Entité " << entity
                                    << " -> pos(" << pos.x << "," <<pos.y << ")\n";
                    }
            }
        }
        private:
            Orchestror& orchestror;
};
