#ifndef HEALTH_SYSTEM_HPP
#define HEALTH_SYSTEM_HPP

#include "../rtype_engine/System.hpp"
#include "../rtype_engine/Orchestror.hpp"

class HealthSystem : public System {
private:
    Orchestror& orchestr;

public:
    HealthSystem(Orchestror& o) : orchestr(o) {}

    void update(float dt) override {
        for (auto entity : entities) {
            if (orchestr.hasComponent<Health>(entity)) {
                auto& health = orchestr.getComponent<Health>(entity);
                // Logique de santé (régénération, dégâts over time, etc.)
                // health.hp -= 1; // Exemple
            }
        }
    }
};

#endif