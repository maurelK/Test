#pragma once
#include "base.hpp"
#include "System.hpp"
#include <memory>
#include <unordered_map>
#include <vector>
#include <typeindex>
#include "Components.hpp"


class SystemManager {
private:
    std::unordered_map<std::type_index, std::shared_ptr<System>> systems;
    std::unordered_map<std::type_index, Signature> systemSignatures;


public:
    template<typename T, typename... Args>
    std::shared_ptr<T> registerSystem(Args&&... args) {
        auto type = std::type_index(typeid(T));
        if (systems.find(type) != systems.end()) {
            return std::dynamic_pointer_cast<T>(systems[type]);
        }
        
        auto system = std::make_shared<T>(std::forward<Args>(args)...);
        systems[type] = system;
        return system;
    }

    template<typename T>
    void setSignature(Signature signature) {
        auto type = std::type_index(typeid(T));
                systemSignatures[type] = signature;

    }

    void entityDestroyed(Entity entity) {
        for (auto& pair : systems) {
            pair.second->entities.erase(entity);
        }
    }

    void entitySignatureChanged(Entity entity, const Signature& signature) {
        for (auto& pair : systems) {
            auto const& type = pair.first;
            auto& system = pair.second;
            auto const& systemSignature = systemSignatures[type];
             if ((signature & systemSignature) == systemSignature) {
                system->entities.insert(entity);
            } else {
                system->entities.erase(entity);
        }
    }
}

    void update(float dt) {
        for (auto& pair : systems) {
            pair.second->update(dt);
        }
    }
    template<typename T>
    std::shared_ptr<T> getSystem() {
        auto type = std::type_index(typeid(T));
        auto it = systems.find(type);
        if (it != systems.end()) {
            return std::dynamic_pointer_cast<T>(it->second);
        }
        return nullptr;
    }
};