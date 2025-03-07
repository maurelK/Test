#include "OutputComponent.hpp"

nts::OutputComponent::OutputComponent() : linkedComponent(nullptr), linkedPin(0) {}

void nts::OutputComponent::simulate(std::size_t tick) {
    (void)tick;
}

void nts::OutputComponent::setLink(std::size_t pin, nts::IComponent &other, std::size_t otherPin) {
    if (pin != 1) {
        throw std::runtime_error("OutputComponent can only be linked at pin 1");
    }
    
    linkedComponent = &other;
    linkedPin = otherPin;

    std::cout << "[DEBUG] OutputComponent linked to " << &other 
              << " on pin " << otherPin << std::endl;
}
