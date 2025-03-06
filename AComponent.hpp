#ifndef ACOMPONENT_HPP_
#define ACOMPONENT_HPP_

#include "IComponent.hpp"
#include <map>

namespace nts {

class AComponent : public IComponent {
protected:
    std::map<std::size_t, std::pair<IComponent *, std::size_t>> _links;

public:
    virtual ~AComponent() = default;
    
    void setLink(std::size_t pin, IComponent &other, std::size_t otherPin) override;
    Tristate getLink(std::size_t pin) const;
};

}

#endif
