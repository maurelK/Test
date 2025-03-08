#ifndef FalseCOMPONENT_HPP_
#define FalseCOMPONENT_HPP_

#include <iostream>
#include "AComponent.hpp"

namespace nts {
class FalseComponent : public AComponent {
public:
    FalseComponent();
    nts::Tristate compute(std::size_t pin) override;
    void simulate(std::size_t tick) override;
    std::string getType() const override { return "false"; }
};
}

#endif