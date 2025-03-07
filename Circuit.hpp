#pragma once
#include "IComponent.hpp"
#include <unordered_map>
#include <memory>
#include <string>
#include <vector>


class Circuit {
public:
    Circuit();

    void loadFromFile(const std::string &filename);

    void simulate();

    void display(size_t tick) const;

    void addComponent(const std::string &name, const std::string &type);

    nts::IComponent* getComponent(const std::string &name);

    // Méthodes pour démarrer la simulation
    void startSimulation();
    bool isSimulationStarted() const;

private:
    std::vector<std::string> inputOrder;
    std::unordered_map<std::string, std::unique_ptr<nts::IComponent>> components;
    bool simulationStarted = false;  // Variable pour savoir si la simulation a commencé
};

