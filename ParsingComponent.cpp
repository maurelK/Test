#include "ParsingComponent.hpp"
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
        line = trim(line);
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


int ParsingComponent::elemExtractChipset(Circuit &circuit)
{
    std::string line, type, name;
    while (std::getline(ChipsetSection, line)) {
        std::istringstream iss(line);
        iss >> type >> name;
        
        if (name.empty() || type.empty()) {
            std::cerr << "Error: Invalid chipset format in line: " << line << std::endl;
            return 84;
        }
        if (circuit.findComponent(name)) {
            continue;
        }
        auto component = Factory::createComponent(type);
        if (!component) {
            std::cerr << "Error: Unknown component type '" << type << "'." << std::endl;
            return 84;
        }
        circuit.addComponent(name, std::move(component));
    }
    return 0;
}


int ParsingComponent::elemExtractLinks(Circuit &circuit)
{
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

        if (!component1) {
            std::cerr << "Unknow component name '" 
                      << comp1_name << "'." <<std::endl;
            return 84;
        }
        if (!component1) {
            std::cerr << "Unknow component name '" 
                      << comp1_name << "'." <<std::endl;
            return 84;
        }
        component1->setLink(pin1, *component2, pin2);
        component2->setLink(pin2, *component1, pin1);

        //auto linkedComponent = component1->compute(pin1);
        //std::cout << "[DEBUG] Verification: " << comp1_name << " now computes pin " 
        //          << pin1 << " as " << linkedComponent << std::endl;
    }
    return 0;
}
