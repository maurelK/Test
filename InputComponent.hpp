#ifndef INPUTCOMPONENT_HPP_
#define INPUTCOMPONENT_HPP_

#include "AComponent.hpp"

namespace nts {

class InputComponent : public AComponent {
private:
    Tristate state;
    Tristate next_state;
    IComponent* linkedComponent;
    std::size_t linkedPin;

public:
    InputComponent();
    void setValue(Tristate value);
    void simulate(std::size_t tick) override;
    void setLink(std::size_t pin, IComponent &other, std::size_t otherPin) override;
    Tristate compute(std::size_t pin) override;
    std::string getType() const override { return "input"; }
};

}

#endif
