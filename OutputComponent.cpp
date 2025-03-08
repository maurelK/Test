#include "OutputComponent.hpp"

nts::OutputComponent::OutputComponent() {}

void nts::OutputComponent::simulate(std::size_t tick) {
    (void)tick;
}

//void nts::OutputComponent::setLink(std::size_t pin, nts::IComponent &other, std::size_t otherPin) {
//    if (pin != 1) {
//        throw std::runtime_error("OutputComponent can only be linked at pin 1");
//    }
//
//    linkedComponent = &other;
//    linkedPin = otherPin;
//
//    std::cout << "[DEBUG] OutputComponent linked to " << other.getType() 
//              << " at pin " << otherPin << " (Memory: " << &other << ")" << std::endl;
//}

void nts::OutputComponent::setLink(std::size_t pin, nts::IComponent &other, std::size_t otherPin) {
    if (pin == 1) {
        links[1] = {&other, otherPin};
    } else {
        std::cerr << "Error: Invalid pin " << pin << " for Output" << std::endl;
    }
}

//nts::Tristate nts::OutputComponent::compute(std::size_t pin) {
//    if (pin != 1) {
//        throw std::runtime_error("OutputComponent only has pin 1.");
//    }
//
//    if (!linkedComponent) {
//        return nts::Undefined;
//    }
/////c'est ici le probleme je pense je pense que la bail ne prend pas le retour de AndComponent
//    return linkedComponent->compute(linkedPin);
//}
//



nts::Tristate nts::OutputComponent::compute(std::size_t pin) {
    if (links.find(1) == links.end()) {
        return nts::Undefined;
    }
    nts::Tristate result = links[1].first->compute(links[1].second);
    state = result;
    return result;
}