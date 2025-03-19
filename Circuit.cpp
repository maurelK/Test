#include "Circuit.hpp"

void Circuit::addComponent(const std::string &name, std::unique_ptr<nts::IComponent> component) 
{
    _components[name] = std::move(component);
    _order.push_back(name);

}

nts::IComponent *Circuit::findComponent(const std::string &name)
{
    auto it = _components.find(name);
    return (it != _components.end()) ? it->second.get() : nullptr;
}

void Circuit::simulate(std::size_t tick = 1)
{
    currentTick += tick;

    // Simulate all components
    for (auto &[name, component] : _components) {
        component->simulate(currentTick);
    }

    // Update outputs  ok ?
    for (auto &[name, component] : _components) {
        if (component->getType() == "output") {
            component->compute(1);
        }
    }
}

void Circuit::display()
{
    std::cout << "tick: " << currentTick << std::endl;

    std::cout << "input(s):" << std::endl;
    for (const auto &name : _order) {  // On suit l'ordre d'ajout my gee
        auto it = _components.find(name);
        if (it != _components.end() && (it->second->getType() == "input" || it->second->getType() == "clock")) {
            nts::Tristate value = it->second->compute(1);
            std::cout << "  " << name << ": " << (value == nts::True ? "1" : value == nts::False ? "0" : "U") << std::endl;
        }
    }

    std::cout << "output(s):" << std::endl;
    for (const auto &name : _order) {
        auto it = _components.find(name);
        if (it != _components.end() && it->second->getType() == "output") {
            nts::Tristate value = it->second->compute(1);
            std::cout << "  " << name << ": " << (value == nts::True ? "1" : value == nts::False ? "0" : "U") << std::endl;
        }
    }
}


