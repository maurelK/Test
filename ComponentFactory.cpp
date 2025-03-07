/*
** EPITECH PROJECT, 2025
** rr
** File description:
** eer
*/

#include "ComponentFactory.hpp"
#include <stdexcept>

namespace nts {
    std::unique_ptr<IComponent> ComponentFactory::createComponent(const std::string &type) {
        if (type == "and" || type == "4081")
            return std::make_unique<AndGate>();
        if (type == "or" || type == "4071")
            return std::make_unique<OrGate>();
        if (type == "xor" || type == "4030")
            return std::make_unique<XorGate>();
        if (type == "input")
            return std::make_unique<Input>();
        if (type == "output")
            return std::make_unique<Output>();

        return nullptr;
    }
}
