#ifndef NANDCOMPONENT_HPP_
#define NANDCOMPONENT_HPP_

#include "AComponent.hpp"

namespace nts {

class NandComponent : public AComponent {
public:
    NandComponent();
    Tristate compute(std::size_t pin) override;
    void simulate(std::size_t tick) override;
    std::string getType() const override { return "nand"; }
};

}

#endif
