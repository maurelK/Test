#include "ClockComponent.hpp"

#include "ClockComponent.hpp"

nts::ClockComponent::ClockComponent() : state(nts::Undefined), nextState(nts::False), manualOverride(false) {}

void nts::ClockComponent::simulate(std::size_t tick) {
    (void)tick;

    if (manualOverride) {
        state = nextState;
        nextState = (state == nts::True) ? nts::False : nts::True;
        manualOverride = false;
    } else if (state != nts::Undefined) {
        state = nextState;
        nextState = (state == nts::True) ? nts::False : nts::True;
    }
}

nts::Tristate nts::ClockComponent::compute(std::size_t pin)
{
    if (pin == 1) {
        return state;
    }
    return nts::Undefined;
}

void nts::ClockComponent::setValue(nts::Tristate newValue) {
    nextState = newValue;
    manualOverride = true;
}
