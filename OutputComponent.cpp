#include "OutputComponent.hpp"

nts::OutputComponent::OutputComponent() {}

void nts::OutputComponent::simulate(std::size_t tick) {
    (void)tick;
}

void nts::OutputComponent::setLink(std::size_t pin, nts::IComponent &other, std::size_t otherPin)
{
    if (pin == 1) {
        links[1] = {&other, otherPin};
    } else {
        std::cerr << "Error: Invalid pin " << pin << " for Output" << std::endl;
    }
}

nts::Tristate nts::OutputComponent::compute(std::size_t pin) {
    if (pin == 1) {
        if (links.find(1) == links.end()) {
            return nts::Undefined;
        }
        nts::Tristate value = links[1].first->compute(links[1].second);
        state = value;
        return value;
    }
    return nts::Undefined;
}