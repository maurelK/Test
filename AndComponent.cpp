#include "AndComponent.hpp"

nts::AndComponent::AndComponent() {}

nts::Tristate nts::AndComponent::compute(std::size_t pin) {
    if (pin != 3) return nts::Undefined;

    nts::Tristate in1 = getLink(1);
    nts::Tristate in2 = getLink(2);

    if (in1 == nts::Undefined || in2 == nts::Undefined) {
        return nts::Undefined;
    } else if (in1 == nts::True && in2 == nts::True) {
        return nts::True;
    } else {
        return nts::False;
    }
}

void nts::AndComponent::simulate(std::size_t tick) {
    (void)tick;
}
