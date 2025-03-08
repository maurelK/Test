#ifndef CIRCUIT_HPP_
#define CIRCUIT_HPP_

#include "IComponent.hpp"
#include <unordered_map>
#include <memory>
#include <iostream>
#include <vector>

class Circuit {
private:
    std::unordered_map<std::string, std::unique_ptr<nts::IComponent>> _components;
    std::size_t currentTick = 0;
    std::vector<std::string> _order;


public:
    void addComponent(const std::string &name, std::unique_ptr<nts::IComponent> component);
    nts::IComponent *findComponent(const std::string &name);
    void simulate(std::size_t tick);
    void display();
    const std::unordered_map<std::string, std::unique_ptr<nts::IComponent>>& getComponents() const {
        return _components;
    }
};

#endif
