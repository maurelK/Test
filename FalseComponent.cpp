#include "FalseComponent.hpp"

nts::FalseComponent::FalseComponent()
{
}

nts::Tristate nts::FalseComponent::compute(std::size_t pin)
{
    if (pin == 1) {
        return nts::False;
    }
    return nts::Undefined;
}

void nts::FalseComponent::simulate(std::size_t tick)
{
    (void)tick; 
}