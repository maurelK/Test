#include "NorComponent.hpp"



nts::NorComponent::NorComponent() {}

nts::Tristate nts::NorComponent::compute(std::size_t pin)
{
    if (pin == 3 || pin == 4 || pin == 10 || pin == 11) {
        std::size_t inputPin1, inputPin2;
        if (pin == 3) {
            inputPin1 = 1;
            inputPin2 = 2;
        } else if (pin == 4) {
            inputPin1 = 5;
            inputPin2 = 6;
        } else if (pin == 10) {
            inputPin1 = 8;
            inputPin2 = 9;
        } else if (pin == 11) {
            inputPin1 = 12;
            inputPin2 = 13;
        } else {
            return nts::Undefined;
        }
        nts::Tristate a = (links.find(inputPin1) != links.end()) ? links[inputPin1].first->compute(links[inputPin1].second) : nts::Undefined;
        nts::Tristate b = (links.find(inputPin2) != links.end()) ? links[inputPin2].first->compute(links[inputPin2].second) : nts::Undefined;
        if (a == nts::Undefined || b == nts::Undefined) {
            if (a == nts::True || b == nts::True) {
                return nts::False;
            } else {
                return nts::Undefined;
            }
        }
        return (a == nts::False && b == nts::False) ? nts::True : nts::False;
    }
    return nts::Undefined;
}

void nts::NorComponent::simulate(std::size_t tick)
{
    (void)tick;
    compute(3);
    compute(4);
    compute(10);
    compute(11);
}
