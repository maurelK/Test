#include "NorComponent.hpp"


nts::NorComponent::NorComponent() {}


nts::Tristate nts::NorComponent::compute(std::size_t pin) {
    if (pin != 3) return nts::Undefined;

    nts::Tristate a = (links.find(1) != links.end()) ? links[1].first->compute(links[1].second) : nts::Undefined;
    nts::Tristate b = (links.find(2) != links.end()) ? links[2].first->compute(links[2].second) : nts::Undefined;

    if (a == nts::Undefined || b == nts::Undefined)
        return nts::Undefined;

    return (a == nts::False && b == nts::False) ? nts::True : nts::False;
}

void nts::NorComponent::simulate(std::size_t tick) {
    (void)tick;
}