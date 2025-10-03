#pragma once
#include "base.hpp"
#include <array>
#include <unordered_map>
#include <typeinfo>
#include <typeindex>

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
        
        size_t indexOfRemovedEntity = entityToIndex[entity];
        size_t indexOfLastElement = size - 1;
        componentArray[indexOfRemovedEntity] = componentArray[indexOfLastElement];
        Entity entityOfLastElement = indexToEntity[indexOfLastElement];
        entityToIndex[entityOfLastElement] = indexOfRemovedEntity;
        indexToEntity[indexOfRemovedEntity] = entityOfLastElemenast;
        entityToIndex.erase(entity);
        indexToEntity.erase(indexOfLastElement);
        size--;
    }

    T& getData(Entity entity) {
        return componentArray[entityToIndex[entity]];
    }

    bool hasData(Entity entity) const {
        return entityToIndex.find(entity) != entityToIndex.end();
    }

    void entityDestroyed(Entity entity) override {
        if (hasData(entity)) removeData(entity);
    }

    auto getAllEntities() const {
        std::vector<Entity> entities;
        for (const auto& pair : entityToIndex) {
            entities.push_back(pair.first);
        }
        return entities;
    }

    template<typename T>
    std::shared_ptr<ComponentStorage<T>> getComponentStorage() {
        auto type = std::type_index(typeid(T));
        return std::static_pointer_cast<ComponentStorage<T>>(componentStorages[type]);
    }
};