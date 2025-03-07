/*
** EPITECH PROJECT, 2025
** drz
** File description:
** rzrz
*/

#include "Circuit.hpp"
#include "ComponentFactory.hpp"
#include "Input.hpp"
#include "Output.hpp"
#include <fstream>
#include <iostream>
#include <sstream>
#include <algorithm>
#include <cctype>
#include <stdexcept>

static inline std::string trim(const std::string &s) {
    auto start = s.begin();
    while (start != s.end() && std::isspace(*start)) {
        ++start;
    }
    auto end = s.end();
    do {
        --end;
    } while (std::distance(start, end) > 0 && std::isspace(*end));
    return std::string(start, end + 1);
}

Circuit::Circuit() {}

nts::IComponent* Circuit::getComponent(const std::string &name) {
    auto it = components.find(name);
    return (it != components.end()) ? it->second.get() : nullptr;
}

void Circuit::addComponent(const std::string &name, const std::string &type) {
    components[name] = nts::ComponentFactory::createComponent(type);
    if (dynamic_cast<Input*>(components[name].get())) {
        inputOrder.push_back(name);  // Enregistre l'entrée dans l'ordre
    }
}

void Circuit::loadFromFile(const std::string &filename) {
    std::ifstream file(filename);
    if (!file.is_open()) {
        std::cerr << "Error: Cannot open file " << filename << std::endl;
        exit(84);
    }

    std::string line;
    bool readingChipsets = false;
    bool readingLinks = false;

    while (std::getline(file, line)) {
        line = trim(line);

        if (line.empty() || line[0] == '#')
            continue;

        if (line == ".chipsets:") {
            readingChipsets = true;
            readingLinks = false;
            continue;
        }
        if (line == ".links:") {
            readingChipsets = false;
            readingLinks = true;
            continue;
        }

        if (readingChipsets) {
            std::istringstream iss(line);
            std::string type, name;
            if (!(iss >> type >> name)) {
                std::cerr << "Error: Invalid chipset format: " << line << std::endl;
                exit(84);
            }
            addComponent(name, type);
        }
        else if (readingLinks) {
            size_t pos1 = line.find(':');
            size_t pos2 = line.find(' ', pos1);
            size_t pos3 = line.find(':', pos2 + 1);
            if (pos1 == std::string::npos || pos2 == std::string::npos || pos3 == std::string::npos) {
                std::cerr << "Error: Invalid link format: " << line << std::endl;
                exit(84);
            }
            std::string comp1 = line.substr(0, pos1);
            std::string pin1 = line.substr(pos1 + 1, pos2 - pos1 - 1);
            std::string comp2 = line.substr(pos2 + 1, pos3 - pos2 - 1);
            std::string pin2 = line.substr(pos3 + 1);
            nts::IComponent *c1 = getComponent(comp1);
            nts::IComponent *c2 = getComponent(comp2);

            if (!c1) {
                std::cerr << "Unknow component name '" << comp1 << "'." << std::endl;
                exit(84);
            }
            if (!c2) {
                std::cerr << "Unknow component name '" << comp2 << "'." << std::endl;
                exit(84);
            }

            try {
                int p1 = std::stoi(pin1);
                int p2 = std::stoi(pin2);
                c1->setLink(p1, *c2, p2);
                c2->setLink(p2, *c1, p1);
            } catch (const std::exception &e) { 
                exit(84);
            }
        }
        else {
            std::cerr << "Error: Unexpected line outside of any section: " << line << std::endl;
            exit(84);
        }
    }
    file.close();
}

void Circuit::simulate() {
    for (auto& comp : components) {
        if (dynamic_cast<Input*>(comp.second.get()) == nullptr) { 
            comp.second->compute(0);
        }
    }
    simulationStarted = true;
}

void Circuit::display(size_t tick) const {
    std::cout << "tick: " << tick << std::endl;

    std::cout << "input(s):" << std::endl; // Added two spaces here
    for (const auto &name : inputOrder) {
        auto it = components.find(name);
        if (it != components.end()) {
            nts::Tristate result = it->second->compute(tick);
            std::cout << "  " << name << ": "; // Added two spaces here
            std::cout << (result == nts::Undefined ? "U" : std::to_string(result)) << std::endl;
        }
    }
    

    std::cout << "output(s):" << std::endl; // Added two spaces here
    if (!simulationStarted) {
        for (const auto &comp : components) {
            if (dynamic_cast<const Output*>(comp.second.get())) {
                std::cout << "  " << comp.first << ": U" << std::endl; // Added two spaces here
            }
        }
    } else {
        for (const auto &comp : components) {
            if (dynamic_cast<const Output*>(comp.second.get())) {
                nts::Tristate result = comp.second->compute(tick);
                std::cout << "  " << comp.first << ": "; // Added two spaces here
                std::cout << (result == nts::Undefined ? "U" : std::to_string(result)) << std::endl;
            }
        }
    }
}


void Circuit::startSimulation() {
    simulationStarted = true;
}

bool Circuit::isSimulationStarted() const {
    return simulationStarted;
}
