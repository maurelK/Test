#ifndef TRUECOMPONENT_HPP_
#define TRUECOMPONENT_HPP_

#include <iostream>
#include "AComponent.hpp"

namespace nts {
class TrueComponent : public AComponent {
public:
    TrueComponent();
    nts::Tristate compute(std::size_t pin) override;
    void simulate(std::size_t tick) override;
    std::string getType() const override { return "true"; }
};
}

#endif