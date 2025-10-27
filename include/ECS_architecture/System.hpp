#include "base.hpp"
#include <unordered_set>
#include "Component_Manager.hpp"


class System {
public:
    std::unordered_set<Entity> entities;
    virtual ~System() = default;
    virtual void update(float dt) = 0;
    
    void addEntity(Entity entity) { entities.insert(entity); }
    void removeEntity(Entity entity) { entities.erase(entity); }
    bool hasEntity(Entity entity) const { return entities.find(entity) != entities.end(); }
};