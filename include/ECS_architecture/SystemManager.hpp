#ifndef SYSTEM_MANAGER_HPP
#define SYSTEM_MANAGER_HPP

#pragma once
#include "base.hpp"
#include "System.hpp"
#include <memory>
#include <unordered_map>
#include <vector>
#include <typeindex>
#include "Component_Manager.hpp"


class SystemManager {
private:
    std::unordered_map<std::type_index, std::shared_ptr<System>> systems;
    std::unordered_map<std::type_index, Signature> systemSignatures;


public:
    template<typename T, typename... Args>
    std::shared_ptr<T> registerSystem(Args&&... args);

    template<typename T>
    void setSignature(Signature signature);
    void entityDestroyed(Entity entity);
    void entitySignatureChanged(Entity entity, const Signature& signature);
    void update(float dt);
    template<typename T>
    std::shared_ptr<T> getSystem();
};

#endif