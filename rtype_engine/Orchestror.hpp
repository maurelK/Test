// core/Coordinator.hpp
#pragma once
#include "EntityManager.hpp"
#include "Component_storage.hpp"
#include "SystemManager.hpp"
#include <memory>
#include <unordered_map>

class Coordinator {
private:
    std::unique_ptr<EntityManager> entityManager;
    std::unique_ptr<SystemManager> systemManager;
    std::unordered_map<std::type_index, std::shared_ptr<IComponentStorage>> componentStorages;

public:
    void init() {
        entityManager = std::make_unique<EntityManager>();
        systemManager = std::make_unique<SystemManager>();
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
        componentStorages[type] = std::make_shared<ComponentStorage<T>>();
    }




    void addComponent(Entity entity, T component) {
    // 1. Ajout du composant dans le storage
    getComponentStorage<T>()->insertData(entity, component);

    // 2. Mise à jour de la signature de l'entité
    Signature signature = entityManager->getSignature(entity);
    signature.set(getComponentType<T>(), true);  // active le bit correspondant
    entityManager->setSignature(entity, signature);

    // 3. Notifier le SystemManager pour mettre à jour la liste des entités dans chaque système
    systemManager->entitySignatureChanged(entity, signature);
    }




    template<typename T>
    void removeComponent(Entity entity) {
        // 1. Supprimer le composant du storage
        getComponentStorage<T>()->removeData(entity);

        // 2. Mise à jour de la signature de l'entité
        Signature signature = entityManager->getSignature(entity);
        signature.set(getComponentType<T>(), false); // désactive le bit
        entityManager->setSignature(entity, signature);

        // 3. Notifier le SystemManager
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

    // Gestion des systèmes
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
    std::shared_ptr<ComponentStorage<T>> getComponentStorage();
};