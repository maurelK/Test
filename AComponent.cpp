#include "AComponent.hpp"
nts::AComponent::AComponent() {}

void nts::AComponent::setLink(std::size_t pin, nts::IComponent &other, std::size_t otherPin)
{
    if (links.find(pin) != links.end()) {
        throw std::runtime_error("Error: Pin " + std::to_string(pin) + " is already linked.");
    }
    
    links[pin] = { &other, otherPin };

    //std::cout << "[DEBUG] " << this->getType() << " linked pin " << pin
    //          << " to " << other.getType() << " pin " << otherPin << std::endl;
}



nts::Tristate nts::AComponent::getLink(std::size_t pin) const
{
    auto it = links.find(pin);
    if (it == links.end() || !it->second.first) {
        std::cout << "[ERROR] getLink(): Pin " << pin << " is not linked! (Component: " 
                  << this->getType() << ")" << std::endl;
        return nts::Undefined;
    }
    return it->second.first->compute(it->second.second);
}

