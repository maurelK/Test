#ifndef ACOMPONENT_HPP_
#define ACOMPONENT_HPP_

#include "IComponent.hpp"
#include <unordered_map>

namespace nts {

class AComponent : public IComponent {
protected:
    std::unordered_map<std::size_t, std::pair<IComponent *, std::size_t>> links;

public:
    AComponent();
    
    void setLink(std::size_t pin, IComponent &other, std::size_t otherPin) override;
    Tristate getLink(std::size_t pin) const;
    
};

}

#endif
