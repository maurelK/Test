/*
** EPITECH PROJECT, 2025
** efer
** File description:
** rerer
*/

#pragma once
#include "IComponent.hpp"
#include "AndGate.hpp"
#include "OrGate.hpp"
#include "XorGate.hpp"
#include "Input.hpp"
#include "Output.hpp"
#include <memory>
#include <string>

namespace nts {
    class ComponentFactory {
    public:
        static std::unique_ptr<IComponent> createComponent(const std::string &type);
    };
}
