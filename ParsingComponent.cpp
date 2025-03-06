#include "ParsingComponent.hpp"

int ParsingComponent::readfile(const std::string &filename)
{
    std::ifstream myfile(filename);
    std::string line;

    bool inChipsets = false;
    bool inLinks = false;

    if (!myfile.is_open()) {
        return 84;
    }

    while (std::getline(myfile, line)) {
        if (line.find(".chipsets:") != std::string::npos) {
            inChipsets = true;
            inLinks = false;
            continue;
        }
        if (line.find(".links:") != std::string::npos) {
            inLinks = true;
            inChipsets = false;
            continue;
        }
        if (inChipsets) {
            ChipsetSection << line << '\n';
        } else if (inLinks) {
            LinksSection << line << '\n';
        }
    }

    myfile.close();

    //std::cout << "Chipsets Section:\n" << ChipsetSection.str() << std::endl;
    return 0;
}


int ParsingComponent::elemExtractChipset(Circuit &circuit) {
    std::string line, type, name;
    while (std::getline(ChipsetSection, line)) {
        std::istringstream iss(line);
        iss >> type >> name;
        
        if (name.empty() || type.empty()) {
            std::cerr << "Error: Invalid chipset format in line: " << line << std::endl;
            return 84;
        }

        // Check for duplicate components
        if (circuit.findComponent(name)) {
            std::cerr << "[ERROR] Component " << name << " is already defined!" << std::endl;
            continue; // Skip duplicate components
        }

        auto component = Factory::createComponent(type);
        if (!component) {
            std::cerr << "Error: Unknown component type '" << type << "'." << std::endl;
            return 84;
        }

        std::cout << "[DEBUG] Adding Component: \"" << name << "\" (" << type << ") at " << component.get() << std::endl;
        circuit.addComponent(name, std::move(component));
    }
    return 0;
}


int ParsingComponent::elemExtractLinks(Circuit &circuit) {
    std::string line;

    while (std::getline(LinksSection, line)) {
        std::istringstream iss(line);
        std::string comp1, comp2;
        std::string pin1_str, pin2_str;

        if (!(iss >> comp1) || comp1.find(':') == std::string::npos ||
            !(iss >> comp2) || comp2.find(':') == std::string::npos) {
            std::cerr << "Error: Invalid link format in line: " << line << std::endl;
            return 84;
        }

        size_t pos1 = comp1.find(':');
        size_t pos2 = comp2.find(':');

        std::string comp1_name = comp1.substr(0, pos1);
        std::string comp2_name = comp2.substr(0, pos2);
        pin1_str = comp1.substr(pos1 + 1);
        pin2_str = comp2.substr(pos2 + 1);

        std::size_t pin1 = std::stoi(pin1_str);
        std::size_t pin2 = std::stoi(pin2_str);

        nts::IComponent* component1 = circuit.findComponent(comp1_name);
        nts::IComponent* component2 = circuit.findComponent(comp2_name);

        if (!component1 || !component2) {
            std::cerr << "[ERROR] Component not found in .chipsets: " 
                      << comp1_name << " or " << comp2_name << std::endl;
            return 84;
        }

        std::cout << "[DEBUG] Calling setLink(): " << comp1_name << ":" << pin1
                  << " --> " << comp2_name << ":" << pin2 << std::endl;

        component1->setLink(pin1, *component2, pin2);
        auto linkedComponent = component1->compute(pin1);
        std::cout << "[DEBUG] Verification: " << comp1_name << " now computes pin " 
                  << pin1 << " as " << linkedComponent << std::endl;
    }
    return 0;
}
