/*
** EPITECH PROJECT, 2025
** Plugins
** File description:
** Handle our differents Plugins
*/

#include <iomanip>
#include <unordered_map>
#include <memory>
#include "../Core/GameEngine.hpp"
class IPlugin {
public:
    virtual ~IPlugin() = default;
    virtual const std::string& getName() const = 0;
    virtual const std::string& getVersion() const = 0;
    
    virtual bool initialize(GameEngine* engine) = 0;
    virtual void update(float deltaTime) = 0;
    virtual void shutdown() = 0;
};

class Plugins_Manager
{
std::unordered_map<std::string, std::shared_ptr<IPlugin>> plugins;
    
public:
    void load_a_Plugin(std::string& path);
    void unload_a_Plugin(std::string& name);
    void update_a_Plugin( float del);
};
