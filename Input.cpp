/*
** EPITECH PROJECT, 2025
** effe
** File description:
** fefe
*/

#include "Input.hpp"

Input::Input() : state(nts::Undefined) {}

nts::Tristate Input::compute(std::size_t pin) {
    //if (pin != 1) return nts::Undefined;
    return state;
}

void Input::simulate(std::size_t tick) {
    (void)tick;
}

void Input::setValue(nts::Tristate value) {
    state = value;
}
