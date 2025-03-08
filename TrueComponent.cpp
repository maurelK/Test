#include "TrueComponent.hpp"

nts::TrueComponent::TrueComponent()
{
}

nts::Tristate nts::TrueComponent::compute(std::size_t pin)
{
    if (pin == 1) {
        return nts::True;
    }
    return nts::Undefined;
}

void nts::TrueComponent::simulate(std::size_t tick)
{
    (void)tick; 
}