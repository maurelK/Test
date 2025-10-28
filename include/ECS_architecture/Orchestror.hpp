#pragma once
#include "EntityManager.hpp"
#include "Component_Manager.hpp"
#include "SystemManager.hpp"
#include <memory>
#include <unordered_map>
#include "Component_Manager.hpp"


class Orchestror {
private:
    std::unique_ptr<EntityManager> entityManager;
    std::unique_ptr<SystemManager> systemManager;
    std::unordered_map<std::type_index, std::shared_ptr<IComponentStorage>> componentStorages;
    
    std::unordered_map<std::type_index, ComponentType> componentTypes;
    ComponentType nextComponentType = 0;

public:
    void init()
    {
        entityManager = std::make_unique<EntityManager>();
        systemManager = std::make_unique<SystemManager>();
    }

    void start()
    {
        systemManager->update(0.0f);
    }

    Entity createEntity() {
        return entityManager->createEntity();
    }

    void destroyEntity(Entity entity) {
        entityManager->destroyEntity(entity);
        for (auto& pair : componentStorages) {
            pair.second->entityDestroyed(entity);
        }
        systemManager->entityDestroyed(entity);
    }

    template<typename T>
    void registerComponent() {
        auto type = std::type_index(typeid(T));
        if (componentTypes.find(type) == componentTypes.end()) {
            componentTypes[type] = nextComponentType++;
        }
        componentStorages[type] = std::make_shared<ComponentStorage<T>>();
    }

    template<typename T>
    ComponentType getComponentType() {
        auto type = std::type_index(typeid(T));
        return componentTypes[type];
    }

    template<typename T>
    void addComponent(Entity entity, T component) {
        getComponentStorage<T>()->insertData(entity, component);
        Signature signature = entityManager->getSignature(entity);
        signature.set(getComponentType<T>(), true);
        entityManager->setSignature(entity, signature);
        systemManager->entitySignatureChanged(entity, signature);
    }

    template<typename T>
    void removeComponent(Entity entity) {
        getComponentStorage<T>()->removeData(entity);
        Signature signature = entityManager->getSignature(entity);
        signature.set(getComponentType<T>(), false);
        entityManager->setSignature(entity, signature);
        systemManager->entitySignatureChanged(entity, signature);
    }

    template<typename T>
    T& getComponent(Entity entity) {
        return getComponentStorage<T>()->getData(entity);
    }

    template<typename T>
    bool hasComponent(Entity entity) {
        return getComponentStorage<T>()->hasData(entity);
    }

    template<typename T, typename... Args>
    std::shared_ptr<T> registerSystem(Args&&... args) {
        return systemManager->registerSystem<T>(std::forward<Args>(args)...);
    }

    template<typename T>
    void setSystemSignature(const Signature& signature) {
        systemManager->setSignature<T>(signature);
    }

    void update(float dt) {
        systemManager->update(dt);
    }

private:
    template<typename T>
    std::shared_ptr<ComponentStorage<T>> getComponentStorage() {
        auto type = std::type_index(typeid(T));
        auto it = componentStorages.find(type);
        if (it != componentStorages.end()) {
            return std::static_pointer_cast<ComponentStorage<T>>(it->second);
        }
        return nullptr;
    }
};

