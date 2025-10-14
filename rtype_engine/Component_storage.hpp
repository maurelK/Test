#ifndef COMPONENT_STORAGE_HPP
#define COMPONENT_STORAGE_HPP

#include "Components.hpp"
#include <array>
#include <unordered_map>
#include <typeindex>
#include <vector>
#include <optional>

class IComponentStorage {
public:
    virtual ~IComponentStorage() = default;
    virtual void entityDestroyed(Entity entity) = 0;
};



template<typename T>
class ComponentStorage : public IComponentStorage {
private:
    std::vector<std::optional<T>> components;
    std::unordered_map<Entity, size_t> entityToIndex;
    std::unordered_map<size_t, Entity> indexToEntity;
    size_t size = 0;

public:
    ComponentStorage() {
        components.resize(MAX_ENTITIES);
    }

    void insertData(Entity entity, T&& component) {
        if (entityToIndex.contains(entity)) {
            throw std::runtime_error("Component already exists for entity");
        }
        if (size >= MAX_ENTITIES) {
            throw std::runtime_error("Max entities reached");
        }

        size_t newIndex = size;
        entityToIndex[entity] = newIndex;
        indexToEntity[newIndex] = entity;
        components[newIndex] = std::move(component); // MOVE
        ++size;
    }

    void removeData(Entity entity) {
        auto it = entityToIndex.find(entity);
        if (it == entityToIndex.end()) return;

        size_t indexOfRemoved = it->second;
        size_t lastIndex = size - 1;

        // Swap avec le dernier élément
        if (indexOfRemoved != lastIndex) {
            components[indexOfRemoved] = std::move(components[lastIndex]);
            Entity lastEntity = indexToEntity[lastIndex];
            entityToIndex[lastEntity] = indexOfRemoved;
            indexToEntity[indexOfRemoved] = lastEntity;
        }

        components[lastIndex].reset();
        entityToIndex.erase(entity);
        indexToEntity.erase(lastIndex);
        --size;
    }

    T& getData(Entity entity) {
        auto it = entityToIndex.find(entity);
        if (it == entityToIndex.end() || !components[it->second].has_value()) {
            throw std::runtime_error("Component not found for entity");
        }
        return components[it->second].value();
    }

    const T& getData(Entity entity) const {
        auto it = entityToIndex.find(entity);
        if (it == entityToIndex.end() || !components[it->second].has_value()) {
            throw std::runtime_error("Component not found for entity");
        }
        return components[it->second].value();
    }

    bool hasData(Entity entity) const {
        auto it = entityToIndex.find(entity);
        return it != entityToIndex.end() && components[it->second].has_value();
    }

    void entityDestroyed(Entity entity) override {
        if (hasData(entity)) {
            removeData(entity);
        }
    }

    std::vector<Entity> getAllEntities() const {
        std::vector<Entity> entities;
        entities.reserve(entityToIndex.size());
        for (const auto& pair : entityToIndex) {
            entities.push_back(pair.first);
        }
        return entities;
    }

    // Iterator support pour les systèmes
    class Iterator {
    private:
        const ComponentStorage* storage;
        size_t index;
        
        void skipEmpty() {
            while (index < storage->size && !storage->components[index].has_value()) {
                ++index;
            }
        }
        
    public:
        Iterator(const ComponentStorage* s, size_t i) : storage(s), index(i) {
            skipEmpty();
        }
        
        Entity operator*() const { return storage->indexToEntity.at(index); }
        Iterator& operator++() { ++index; skipEmpty(); return *this; }
        bool operator!=(const Iterator& other) const { return index != other.index; }
    };
    
    Iterator begin() const { return Iterator(this, 0); }
    Iterator end() const { return Iterator(this, size); }
};

#endif