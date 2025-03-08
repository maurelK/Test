#ifndef NOTCOMPONENT_HPP_
#define NOTCOMPONENT_HPP_

#include "AComponent.hpp"

namespace nts {

class NotComponent : public AComponent {
public:
    NotComponent();
    Tristate compute(std::size_t pin) override;
    void simulate(std::size_t tick) override;
    std::string getType() const override { return "not"; }
};

}

#endif
