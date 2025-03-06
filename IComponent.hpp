#ifndef ICOMPONENT_HPP_
#define ICOMPONENT_HPP_

#include <iostream>
#include <cstddef>

namespace nts {

enum Tristate {
    Undefined = (-1),
    True = 1,
    False = 0
};

class IComponent {
public:
    virtual ~IComponent() = default;
    virtual void simulate(std::size_t tick) = 0;
    virtual Tristate compute(std::size_t pin) = 0;
    virtual void setLink(std::size_t pin, IComponent &other, std::size_t otherPin) = 0;
    virtual std::string getType() const = 0; // To identify component type
};

}

#endif
