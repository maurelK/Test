#include "InputComponent.hpp"

nts::InputComponent::InputComponent() : state(nts::Undefined) {}

void nts::InputComponent::setValue(nts::Tristate value)
{
    state = value;
}

void nts::InputComponent::setLink(std::size_t pin, nts::IComponent &other, std::size_t otherPin)
{
    linkedComponent = &other;
    linkedPin = otherPin;
}

nts::Tristate nts::InputComponent::compute(std::size_t pin)
{
    return state;
}
void nts::InputComponent::simulate(std::size_t tick) {
    (void)tick; // Mark unused parameter

    // Propagate the input value to linked components
    if (linkedComponent) {
        linkedComponent->compute(linkedPin);
    }
}