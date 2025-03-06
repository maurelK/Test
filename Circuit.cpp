#include "Circuit.hpp"

void Circuit::addComponent(const std::string &name, std::unique_ptr<nts::IComponent> component) {
    _components[name] = std::move(component);
}

nts::IComponent *Circuit::findComponent(const std::string &name) {
    auto it = _components.find(name);
    return (it != _components.end()) ? it->second.get() : nullptr;
}

void Circuit::simulate(std::size_t tick = 1) {
    currentTick += tick; // Increment the current tick
    std::cout << "[DEBUG] Running simulation for tick: " << currentTick << std::endl;

    // Propagate signals through the circuit
    for (auto &[name, component] : _components) {
        component->simulate(currentTick);
    }

    // Compute outputs
    for (auto &[name, component] : _components) {
        if (component->getType() == "output") {
            component->compute(1); // Ensure this is the correct pin
        }
    }
}

void Circuit::display() {
    std::cout << "tick: " << currentTick << std::endl;  // Add tick count
    std::cout << "input(s):" << std::endl;
    for (auto &[name, component] : _components) {
        if (component->getType() == "input") {
            std::cout <<"   "<< name << ": " << component->compute(1) << std::endl;
        }
    }
    std::cout << "output(s):" << std::endl;
    for (auto &[name, component] : _components) {
        if (component->getType() == "output") {
            nts::Tristate value = component->compute(1);
            std::cout <<"   "<< name << ": " << (value == nts::True ? "1" : value == nts::False ? "0" : "U") << std::endl;
        }
    }
}
