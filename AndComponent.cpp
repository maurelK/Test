#include "AndComponent.hpp"

nts::AndComponent::AndComponent() {}
nts::Tristate nts::AndComponent::compute(std::size_t pin) {
    if (pin != 3) {
        throw std::runtime_error("[ERROR] AndComponent: Output is only available at pin 3.");
    }

    // Retrieve input values
    nts::Tristate input1 = getLink(1);
    nts::Tristate input2 = getLink(2);

    std::cout << "[DEBUG] AndComponent: Input1=" << input1 << ", Input2=" << input2 << std::endl;

    if (input1 == nts::False || input2 == nts::False) {
        return nts::False;
    }
    if (input1 == nts::Undefined || input2 == nts::Undefined) {
        return nts::Undefined;
    }
    return nts::True;    
}

void nts::AndComponent::simulate(std::size_t tick) {
    (void)tick;
}

void nts::AndComponent::setLink(std::size_t pin, nts::IComponent &other, std::size_t otherPin) {
    if (pin != 3) {
        _links[pin] = { &other, otherPin };
    }

    std::cout << "[DEBUG] AndComponent linking pin " << pin << " to " 
              << other.getType() << " at pin " << otherPin 
              << " (Memory: " << &other << ")" << std::endl;
}
