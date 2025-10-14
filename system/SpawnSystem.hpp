#ifndef SPAWN_SYSTEM_HPP
#define SPAWN_SYSTEM_HPP

#include "../rtype_engine/System.hpp"
#include "../rtype_engine/Orchestror.hpp"

class SpawnSystem : public System {
private:
    Orchestror& orchestr;

public:
    SpawnSystem(Orchestror& o) : orchestr(o) {}

    void update(float dt) override {
        // Logique de spawn automatique peut être ajoutée ici
    }

    Entity spawnEnemy(float x, float y) {
        Entity enemy = orchestr.createEntity();
        orchestr.addComponent(enemy, Position{x, y});
        orchestr.addComponent(enemy, Velocity{-80.f, 0.f});
        orchestr.addComponent(enemy, Health{100});
        return enemy;
    }
};

#endif