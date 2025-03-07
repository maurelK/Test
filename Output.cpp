/*
** EPITECH PROJECT, 2025
** eferf
** File description:
** fefe
*/

#include "Output.hpp"

Output::Output() {}

nts::Tristate Output::compute(std::size_t pin) {
    if (links.find(1) == links.end()) {
        return nts::Undefined;
    }
    nts::Tristate result = links[1].first->compute(links[1].second);
    state = result;
    return result;
}

void Output::simulate(std::size_t tick) {
    (void)tick; 
    compute(1);
}

void Output::setLink(std::size_t pin, nts::IComponent &other, std::size_t otherPin) {
    if (pin == 1) {
        links[1] = {&other, otherPin};
    } else {
        std::cerr << "Error: Invalid pin " << pin << " for Output" << std::endl;
    }
}

nts::IComponent* Output::getLink(std::size_t pin) {
    if (links.find(pin) != links.end()) {
        return links[pin].first;
    }
    return nullptr;
}

void Output::setValue(nts::Tristate value) {
    state = value;
}
