#include "../../include/ECS_architecture/Component_Manager.hpp"
#include "Component_Manager.hpp"

template <typename T>
inline void ComponentStorage<T>::insertData(Entity entity, T component)
{
    if (entityToIndex.find(entity) != entityToIndex.end()) return;
    size_t newIndex = size;
    entityToIndex[entity] = newIndex;
    indexToEntity[newIndex] = entity;
    componentArray[newIndex] = component;
    size++;
}
template <typename T>
void ComponentStorage<T>::removeData(Entity entity)
{
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

template <typename T>
const T &ComponentStorage<T>::getData(Entity entity) const
{
    return componentArray.at(entityToIndex.at(entity));
}

template <typename T>
bool ComponentStorage<T>::hasData(Entity entity) const
{
    return entityToIndex.find(entity) != entityToIndex.end();
}

template <typename T>
void ComponentStorage<T>::entityDestroyed(Entity entity)
{
    if (hasData(entity)) removeData(entity);

}

template <typename T>
std::vector<Entity> ComponentStorage<T>::getAllEntities() const
{
    std::vector<Entity> entities;
    for (const auto& pair : entityToIndex) {
        entities.push_back(pair.first);
    }
    return entities;
}
