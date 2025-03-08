
#ifndef ORCOMPONENT_HPP_
#define ORCOMPONENT_HPP_

#include "AComponent.hpp"

namespace nts {

class OrComponent : public AComponent {
public:
    OrComponent();
    Tristate compute(std::size_t pin) override;
    void simulate(std::size_t tick) override;
    std::string getType() const override { return "or"; }
};

}

#endif