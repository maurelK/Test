#ifndef PROJECTILE_SYSTEM_HPP
#define PROJECTILE_SYSTEM_HPP

#include "../rtype_engine/System.hpp"
#include "../rtype_engine/Orchestror.hpp"

class ProjectileSystem : public System {
private:
    Orchestror& orchestr;

public:
    ProjectileSystem(Orchestror& o) : orchestr(o) {}

    void update(float dt) override {
        std::vector<Entity> toRemove;
        
        for (auto entity : entities) {
            if (!orchestr.hasComponent<Position>(entity) ||
                !orchestr.hasComponent<Velocity>(entity) ||
                !orchestr.hasComponent<Projectile>(entity)) {
                continue;
            }
            
            auto& pos = orchestr.getComponent<Position>(entity);
            auto& vel = orchestr.getComponent<Velocity>(entity);
            auto& proj = orchestr.getComponent<Projectile>(entity);
            
            // Mettre à jour la position
            pos.x += vel.dx * dt;
            pos.y += vel.dy * dt;
            
            // Diminuer la durée de vie
            proj.life -= dt;
            if (proj.life <= 0) {
                toRemove.push_back(entity);
                continue;
            }
            
            // Vérifier les collisions
            for (auto target : orchestr.getLivingEntities()) {
                if (target == proj.owner || target == entity) continue;
                
                if (orchestr.hasComponent<Position>(target) && 
                    orchestr.hasComponent<Health>(target)) {
                    
                    auto& targetPos = orchestr.getComponent<Position>(target);
                    auto& targetHealth = orchestr.getComponent<Health>(target);
                    
                    float dx = pos.x - targetPos.x;
                    float dy = pos.y - targetPos.y;
                    float dist2 = dx * dx + dy * dy;
                    const float collisionRadius2 = 100.0f; // Ajuster selon les besoins
                    
                    if (dist2 <= collisionRadius2) {
                        targetHealth.hp -= proj.damage;
                        toRemove.push_back(entity);
                        std::cout << "[Projectile] Hit! Entity " << target 
                                  << " took " << proj.damage << " damage\n";
                        break;
                    }
                }
            }
        }
        
        // Supprimer les projectiles expirés
        for (auto entity : toRemove) {
            orchestr.destroyEntity(entity);
        }
    }
};

#endif