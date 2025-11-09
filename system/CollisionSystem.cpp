#include "CollisionSystem.hpp"

void CollisionSystem::update(float dt) {
    (void)dt;
    for (auto e1 : entities) {
        for (auto e2 : entities) {
            if (e1 == e2) continue;

            if (!orchestror.hasComponent<Position>(e1) || !orchestror.hasComponent<Position>(e2))
                continue;

            auto& p1 = orchestror.getComponent<Position>(e1);
            auto& p2 = orchestror.getComponent<Position>(e2);

            // Simple collision detection
            if (p1.x == p2.x && p1.y == p2.y) {
                if (orchestror.hasComponent<Health>(e1))
                    orchestror.getComponent<Health>(e1).hp--;

                if (orchestror.hasComponent<Health>(e2))
                    orchestror.getComponent<Health>(e2).hp--;
            }
        }
    }
}
