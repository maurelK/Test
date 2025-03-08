#include "NotComponent.hpp"

nts::NotComponent::NotComponent() {}

nts::Tristate nts::NotComponent::compute(std::size_t pin) {
    if (pin != 2) return nts::Undefined;

    nts::Tristate a = (links.find(1) != links.end()) ? links[1].first->compute(links[1].second) : nts::Undefined;

    if (a == nts::Undefined)
        return nts::Undefined;

    return (a == nts::True) ? nts::False : nts::True;

}

void nts::NotComponent::simulate(std::size_t tick) {
    (void)tick;
}
