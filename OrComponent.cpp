#include "OrComponent.hpp"

nts::OrComponent::OrComponent() {}


nts::Tristate nts::OrComponent::compute(std::size_t pin) {
    if (pin != 3) return nts::Undefined;

    nts::Tristate a = (links.find(1) != links.end()) ? links[1].first->compute(links[1].second) : nts::Undefined;
    nts::Tristate b = (links.find(2) != links.end()) ? links[2].first->compute(links[2].second) : nts::Undefined;

    if (a == nts::True || b == nts::True)
        return nts::True;
    else if (a == nts::False && b == nts::False)
        return nts::False;
    else
        return nts::Undefined;
}


void nts::OrComponent::simulate(std::size_t tick) {
    (void)tick;
}

