/*
** EPITECH PROJECT, 2025
** rr
** File description:
** erre
*/

#include "XorGate.hpp"

XorGate::XorGate() {}

nts::Tristate XorGate::compute(std::size_t pin) {
    if (pin != 3) return nts::Undefined;

    nts::Tristate a = (links.find(1) != links.end()) ? links[1].first->compute(links[1].second) : nts::Undefined;
    nts::Tristate b = (links.find(2) != links.end()) ? links[2].first->compute(links[2].second) : nts::Undefined;

    if (a == nts::Undefined || b == nts::Undefined)
        return nts::Undefined;
    return (a != b) ? nts::True : nts::False;
}


void XorGate::simulate(std::size_t tick) {
    (void)tick;
}
