#ifndef SYSTEM_MANAGER_HPP
#define SYSTEM_MANAGER_HPP

#include "base.hpp"
#include "System.hpp"
#include <memory>
#include <unordered_map>
#include <typeindex>
#include "../../include/ECS_architecture/SystemManager.hpp"

class SystemManager {
private:
    std::unordered_map<std::type_index, std::shared_ptr<System>> systems;
    std::unordered_map<std::type_index, Signature> systemSignatures;

public:
    template<typename T, typename... Args>
    std::shared_ptr<T> registerSystem(Args&&... args) {
        auto type = std::type_index(typeid(T));
        auto system = std::make_shared<T>(std::forward<Args>(args)...);
        systems[type] = system;
        return system;
    }

    template<typename T>
    void setSignature(Signature signature) {
        auto type = std::type_index(typeid(T));
        systemSignatures[type] = signature;
    }

    template<typename T>
    std::shared_ptr<T> getSystem() {
        auto type = std::type_index(typeid(T));
        auto it = systems.find(type);
        if (it != systems.end()) {
            return std::static_pointer_cast<T>(it->second);
        }
        return nullptr;
    }

    void entityDestroyed(Entity entity) {
        for (auto& pair : systems) {
            pair.second->removeEntity(entity);
        }
    }

    void entitySignatureChanged(Entity entity, const Signature& signature) {
        for (auto& pair : systems) {
            auto const& type = pair.first;
            auto& system = pair.second;
            auto const& systemSignature = systemSignatures[type];
            
            if ((signature & systemSignature) == systemSignature) {
                system->addEntity(entity);
            } else {
                system->removeEntity(entity);
            }
        }
    }

    void update(float dt) {
        for (auto& pair : systems) {
            pair.second->update(dt);
        }
    }
};

#endif