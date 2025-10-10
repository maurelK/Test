#pragma once
#include "base.hpp"
#include <vector>
#include <queue>
#include <array>
#include <algorithm>
#include "Components.hpp"


class EntityManager {
private:
    std::queue<Entity> availableEntities;
    std::vector<Entity> livingEntities;
    uint32_t livingEntityCount = 0;
    std::array<Signature, MAX_ENTITIES> signatures;
    



public:
    EntityManager() {
        for (Entity entity = 0; entity < MAX_ENTITIES; ++entity) {
            availableEntities.push(entity);
        }
    }

    Entity createEntity() {        
        Entity id = availableEntities.front();
        availableEntities.pop();
        livingEntities.push_back(id);
        livingEntityCount++;
        signatures[id].reset();
        return id;
    }

    void destroyEntity(Entity entity) {
        auto elem = std::find(livingEntities.begin(), livingEntities.end(), entity);
        if(elem != livingEntities.end()) {
            std::swap(*elem, livingEntities.back());
            livingEntities.pop_back();
            livingEntityCount--;
            availableEntities.push(entity);
            signatures[entity].reset();
        }
    }


    void setSignature(Entity entity, Signature signature) { 
        signatures[entity] = signature;
    }
    
    Signature getSignature(Entity entity) {
        return signatures[entity];
    }

    const std::vector<Entity>& getLivingEntities() const { 
        return livingEntities; 
    }
    
    uint32_t getLivingEntityCount() const { 
        return livingEntityCount; 
    }
};