#ifndef CLOCKCOMPONENT_HPP_
#define CLOCKCOMPONENT_HPP_

#include "AComponent.hpp"

namespace nts {

class ClockComponent : public AComponent {
private:
    
    Tristate state;
    Tristate nextState;
    bool manualOverride = false;

public:
    ClockComponent();
    void simulate(std::size_t tick) override;
    Tristate compute(std::size_t pin) override;
    void setValue(Tristate value);
    std::string getType() const override { return "clock"; }
};

}

#endif