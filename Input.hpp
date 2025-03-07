/*
** EPITECH PROJECT, 2025
** eef
** File description:
** efe
*/

#pragma once
#include "AComponent.hpp"

class Input : public nts::AComponent {
public:
    Input();
    nts::Tristate compute(std::size_t pin) override;
    void simulate(std::size_t tick) override;
    void setValue(nts::Tristate value);
    
private:
    nts::Tristate state;
};
