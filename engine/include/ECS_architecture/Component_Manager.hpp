#ifndef COMPONENT_MANAGER_HPP
#define COMPONENT_MANAGER_HPP

#include <array>
#include <unordered_map>
#include <typeindex>
#include <vector>
#include "base.hpp"

class IComponentStorage {
public:
    virtual ~IComponentStorage() = default;
    virtual void entityDestroyed(Entity entity) = 0;
};

template<typename T>
class ComponentStorage : public IComponentStorage {
private:
    std::array<T, MAX_ENTITIES> componentArray;
    std::unordered_map<Entity, size_t> entityToIndex;
    std::unordered_map<size_t, Entity> indexToEntity;
    size_t size = 0;

public:
    void insertData(Entity entity, T component) {
        if (entityToIndex.find(entity) != entityToIndex.end()) return;
        size_t newIndex = size;
        entityToIndex[entity] = newIndex;
        indexToEntity[newIndex] = entity;
        componentArray[newIndex] = component;
        size++;
    }
    
    void removeData(Entity entity) {
        if (entityToIndex.find(entity) == entityToIndex.end()) return;
        size_t indexOfRemoved = entityToIndex[entity];
        size_t lastIndex = size - 1;
        componentArray[indexOfRemoved] = componentArray[lastIndex];
        Entity lastEntity = indexToEntity[lastIndex];
        entityToIndex[lastEntity] = indexOfRemoved;
        indexToEntity[indexOfRemoved] = lastEntity;
        entityToIndex.erase(entity);
        indexToEntity.erase(lastIndex);
        size--;
    }
    
    T& getData(Entity entity) {
        return componentArray[entityToIndex.at(entity)];
    }
    
    const T& getData(Entity entity) const {
        return componentArray.at(entityToIndex.at(entity));
    }
    
    bool hasData(Entity entity) const {
        return entityToIndex.find(entity) != entityToIndex.end();
    }
    
    void entityDestroyed(Entity entity) override {
        if (hasData(entity)) removeData(entity);
    }
};

#endif