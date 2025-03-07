/*
** EPITECH PROJECT, 2025
** rre
** File description:
** rere
*/

#include "AComponent.hpp"

nts::AComponent::AComponent() {}

void nts::AComponent::setLink(std::size_t pin, nts::IComponent &other, std::size_t otherPin) {
    links[pin] = {&other, otherPin};
}

