#include "AndComponent.hpp"

nts::AndComponent::AndComponent() {}
nts::Tristate nts::AndComponent::compute(std::size_t pin) {
    if (pin != 3) {
        throw std::runtime_error("[ERROR] AndComponent: Output is only available at pin 3.");
    }

    // Retrieve input values
    nts::Tristate input1 = getLink(1);
    nts::Tristate input2 = getLink(2);

    std::cout << "[DEBUG] AndComponent: Input1=" << input1 << ", Input2=" << input2 << std::endl;

    // Compute the AND logic
    if (input1 == nts::Undefined || input2 == nts::Undefined) {
        return nts::Undefined;
    }
    return (input1 == nts::True && input2 == nts::True) ? nts::True : nts::False;
}
void nts::AndComponent::simulate(std::size_t tick) {
    (void)tick;
}
