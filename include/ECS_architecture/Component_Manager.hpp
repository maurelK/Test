#ifndef COMPONENT_MANAGER_HPP
#define COMPONENT_MANAGER_HPP

#include <array>
#include <unordered_map>
#include <typeindex>
#include <vector>
#include <base.hpp>

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
    void insertData(Entity entity, T component);
    void removeData(Entity entity);
    const T& getData(Entity entity) const;
    bool hasData(Entity entity) const;
    void entityDestroyed(Entity entity);
    std::vector<Entity> getAllEntities() const;
};

#endif

