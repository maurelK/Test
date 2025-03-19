#include "NotComponent.hpp"

nts::NotComponent::NotComponent() {}

nts::Tristate nts::NotComponent::compute(std::size_t pin)
{
    std::size_t inputPin;
    if (pin == 2) {
        inputPin = 1;
    } else if (pin == 4) {
        inputPin = 3;
    } else if (pin == 6) {
        inputPin = 5;
    } else if (pin == 8) {
        inputPin = 9;
    } else if (pin == 10) {
        inputPin = 11;
    } else if (pin == 12) {
        inputPin = 13;
    } else {
        return nts::Undefined;
    }
    nts::Tristate a = (links.find(inputPin) != links.end()) ? links[inputPin].first->compute(links[inputPin].second) : nts::Undefined;
    if (a == nts::Undefined) {
        return nts::Undefined;
    }
    return (a == nts::True) ? nts::False : nts::True;
}

void nts::NotComponent::simulate(std::size_t tick) {
    (void)tick;
    compute(2);
    compute(4);
    compute(6);
    compute(8);
    compute(10);
    compute(12);
}
