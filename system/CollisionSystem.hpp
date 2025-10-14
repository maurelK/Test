#ifndef COLLISION_SYSTEM_HPP
#define COLLISION_SYSTEM_HPP

#include "../rtype_engine/System.hpp"
#include "../rtype_engine/Orchestror.hpp"

class CollisionSystem : public System {
private:
    Orchestror& orchestr;

public:
    CollisionSystem(Orchestror& o) : orchestr(o) {}

    void update(float dt) override {
        for (auto entity1 : entities) {
            for (auto entity2 : entities) {
                if (entity1 == entity2) continue;
                
                if (orchestr.hasComponent<Position>(entity1) && 
                    orchestr.hasComponent<Position>(entity2) &&
                    orchestr.hasComponent<Health>(entity1)) {
                    
                    auto& pos1 = orchestr.getComponent<Position>(entity1);
                    auto& pos2 = orchestr.getComponent<Position>(entity2);
                    
                    // Détection de collision simple
                    float dx = pos1.x - pos2.x;
                    float dy = pos1.y - pos2.y;
                    float distance = dx*dx + dy*dy;
                    
                    if (distance < 50.0f) { // Seuil de collision
                        auto& health = orchestr.getComponent<Health>(entity1);
                        health.hp -= 1;
                    }
                }
            }
        }
    }
};

#endif