#ifndef OUTPUTCOMPONENT_HPP_
#define OUTPUTCOMPONENT_HPP_

#include "AComponent.hpp"
#include "AndComponent.hpp"
namespace nts {

class OutputComponent : public AndComponent {
private:
    IComponent* linkedComponent;
    std::size_t linkedPin;

public:
    OutputComponent();
    ~OutputComponent() override = default;
    
    void simulate(std::size_t tick) override;
    //nts::Tristate compute(std::size_t pin) override;
    void setLink(std::size_t pin, IComponent &other, std::size_t otherPin) override;
    std::string getType() const override { return "output"; }
};

}

#endif
