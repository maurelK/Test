/*
** EPITECH PROJECT, 2025
** fte
** File description:
** rere
*/

#pragma once
#include "AComponent.hpp"

class AndGate : public nts::AComponent {
public:
    AndGate();
    nts::Tristate compute(std::size_t pin) override;
    void simulate(std::size_t tick) override;
};
