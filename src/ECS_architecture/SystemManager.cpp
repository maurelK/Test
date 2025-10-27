#include "../../include/ECS_architecture/SystemManager.hpp"
#include "SystemManager.hpp"

template <typename T, typename... Args>
std::shared_ptr<T> SystemManager::registerSystem(Args &&...args)
{
    return std::shared_ptr<T>();
}

template <typename T>
void SystemManager::setSignature(Signature signature)
{
}

template <typename T>
std::shared_ptr<T> SystemManager::getSystem()
{
    return std::shared_ptr<T>();
}

void SystemManager::entityDestroyed(Entity entity)
{
    for (auto& pair : systems) {
            pair.second->entities.erase(entity);
        }
}

void SystemManager::entitySignatureChanged(Entity entity, const Signature &signature)
{
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

void SystemManager::update(float dt)
{
    for (auto& pair : systems) {
        pair.second->update(dt);
    }
}

