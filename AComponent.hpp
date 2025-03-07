/*
** EPITECH PROJECT, 2025
** erre
** File description:
** erre
*/
#pragma once
#include "IComponent.hpp"
#include <unordered_map>

namespace nts {
    class AComponent : public IComponent {
    public:
        AComponent();
        void setLink(std::size_t pin, IComponent &other, std::size_t otherPin) override;

    protected:
        std::unordered_map<std::size_t, std::pair<IComponent *, std::size_t>> links;
    };
}
