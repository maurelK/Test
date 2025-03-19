#ifndef OUTPUTCOMPONENT_HPP_
#define OUTPUTCOMPONENT_HPP_

#include "AComponent.hpp"
#include "AndComponent.hpp"
#include <unordered_map>

namespace nts {

class OutputComponent : public AComponent {
private:
    std::unordered_map<std::size_t, std::pair<nts::IComponent*, std::size_t>> links;
    nts::Tristate state;
    nts::Tristate linkedComponent;

public:
    OutputComponent();
    ~OutputComponent() override = default;
    
    void simulate(std::size_t tick) override;
    nts::Tristate compute(std::size_t pin) override;
    void setLink(std::size_t pin, IComponent &other, std::size_t otherPin) override;
    std::string getType() const override { return "output"; }
    
};

}

#endif
