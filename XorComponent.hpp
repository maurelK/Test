
#ifndef XORCOMPONENT_HPP_
#define XORCOMPONENT_HPP_

#include "AComponent.hpp"

namespace nts {

class XorComponent : public AComponent {
public:
    XorComponent();
    Tristate compute(std::size_t pin) override;
    void simulate(std::size_t tick) override;
    std::string getType() const override { return "xor"; }
};

}

#endif