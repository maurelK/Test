/*
** EPITECH PROJECT, 2025
** erfe
** File description:
** feef
*/

#pragma once
#include "AComponent.hpp"

#ifndef ORGATE_HPP
#define ORGATE_HPP

#include "IComponent.hpp"
#include <map>
#include <utility>

class OrGate : public nts::IComponent {
public:
    OrGate();
    
    nts::Tristate compute(std::size_t pin) override;

    void setLink(std::size_t pin, nts::IComponent &component, std::size_t otherPin) override;

    void simulate(std::size_t tick) override;

private:
    std::map<std::size_t, std::pair<nts::IComponent*, std::size_t>> links;
};

#endif
