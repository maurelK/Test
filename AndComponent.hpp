#ifndef ANDCOMPONENT_HPP_
#define ANDCOMPONENT_HPP_

#include "AComponent.hpp"

namespace nts {

class AndComponent : public AComponent {
public:
    AndComponent();
    Tristate compute(std::size_t pin) override;
    void simulate(std::size_t tick) override;
    void setLink(std::size_t pin, nts::IComponent &other, std::size_t otherPin) override;
    std::string getType() const override { return "4081"; }
};

}

#endif
