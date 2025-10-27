#include "../../include/ECS_architecture/EntityManager.hpp"

EntityManager::EntityManager()
{
    for (Entity entity = 0; entity < MAX_ENTITIES; ++entity) {
            availableEntities.push(entity);
        }
}

Entity EntityManager::createEntity()
{
    Entity id = availableEntities.front();
    availableEntities.pop();
    livingEntities.push_back(id);
    livingEntityCount++;
    signatures[id].reset();
    return id;
}

void EntityManager::destroyEntity(Entity entity)
{
    auto elem = std::find(livingEntities.begin(), livingEntities.end(), entity);
    if(elem != livingEntities.end()) {
        std::swap(*elem, livingEntities.back());
        livingEntities.pop_back();
        livingEntityCount--;
        availableEntities.push(entity);
        signatures[entity].reset();
    }
}

void EntityManager::setSignature(Entity entity, Signature signature)
{
    signatures[entity] = signature;

}

Signature EntityManager::getSignature(Entity entity)
{
    return signatures[entity];
}

const std::vector<Entity> &EntityManager::getLivingEntities() const
{
    return livingEntities;
}

uint32_t EntityManager::getLivingEntityCount() const
{
    return livingEntityCount;
}
