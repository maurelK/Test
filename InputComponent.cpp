#include "InputComponent.hpp"

nts::InputComponent::InputComponent() : state(nts::Undefined), next_state(nts::Undefined) {}

void nts::InputComponent::setValue(nts::Tristate value) {
    next_state = value;
}

void nts::InputComponent::setLink(std::size_t pin, nts::IComponent &other, std::size_t otherPin)
{
    (void)pin;
    linkedComponent = &other;
    linkedPin = otherPin;
}

void nts::InputComponent::simulate(std::size_t tick) {
    (void)tick;
    state = next_state;
}

nts::Tristate nts::InputComponent::compute(std::size_t pin) {
    (void)pin;
    return state;
}
