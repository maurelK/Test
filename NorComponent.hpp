#ifndef NORCOMPONENT_HPP_
#define NORCOMPONENT_HPP_

#include "AComponent.hpp"

namespace nts {

class NorComponent : public AComponent {
public:
    NorComponent();
    Tristate compute(std::size_t pin) override;
    void simulate(std::size_t tick) override;
    std::string getType() const override { return "nor"; }
};

}

#endif
