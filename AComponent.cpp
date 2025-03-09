#include "AComponent.hpp"
nts::AComponent::AComponent() {}

void nts::AComponent::setLink(std::size_t pin, nts::IComponent &other, std::size_t otherPin) {
    links[pin] = {&other, otherPin};
}


nts::Tristate nts::AComponent::getLink(std::size_t pin) const {
    auto it = links.find(pin);
    if (it == links.end() || !it->second.first) {
        std::cout << "[ERROR] getLink(): Pin " << pin << " is not linked! (Component: " 
                  << this->getType() << ")" << std::endl;
        return nts::Undefined;
    }

    std::cout << "[DEBUG] getLink(): Pin " << pin << " calls compute on " 
              << it->second.first->getType() << " at pin " << it->second.second 
              << " (Memory: " << it->second.first << ")" << std::endl;

    return it->second.first->compute(it->second.second);
}

