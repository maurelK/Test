#pragma once
#include "base.hpp"
#include <vector>
#include <queue>
#include <array>
#include <algorithm>
#include "Component_Manager.hpp"


class EntityManager {
private:
    std::queue<Entity> availableEntities;
    std::vector<Entity> livingEntities;
    uint32_t livingEntityCount = 0;
    std::array<Signature, MAX_ENTITIES> signatures;
    
public:
    EntityManager();
    Entity createEntity();
    void destroyEntity(Entity entity);
    void setSignature(Entity entity, Signature signature);
    Signature getSignature(Entity entity);
};