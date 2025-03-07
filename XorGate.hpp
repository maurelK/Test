/*
** EPITECH PROJECT, 2025
** refef
** File description:
** fefe
*/

#pragma once
#include "AComponent.hpp"

class XorGate : public nts::AComponent {
public:
    XorGate();
    nts::Tristate compute(std::size_t pin) override;
    void simulate(std::size_t tick) override;
};
