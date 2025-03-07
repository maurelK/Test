/*
** EPITECH PROJECT, 2025
** efe
** File description:
** fef
*/

#ifndef OUTPUT_HPP
#define OUTPUT_HPP

#include "IComponent.hpp"
#include <unordered_map>

class Output : public nts::IComponent {
public:
    Output();
    nts::Tristate compute(std::size_t pin) override;
    void simulate(std::size_t tick) override;
    void setLink(std::size_t pin, nts::IComponent &other, std::size_t otherPin) override;
    void setValue(nts::Tristate value);
    nts::IComponent* getLink(std::size_t pin);

private:
    std::unordered_map<std::size_t, std::pair<nts::IComponent*, std::size_t>> links;
    nts::Tristate state;
};

#endif
