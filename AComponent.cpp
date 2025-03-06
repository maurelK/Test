#include "AComponent.hpp"

void nts::AComponent::setLink(std::size_t pin, nts::IComponent &other, std::size_t otherPin) {
    if (_links.find(pin) != _links.end()) {
        throw std::runtime_error("Error: Pin " + std::to_string(pin) + " is already linked.");
    }
    _links[pin] = { &other, otherPin };
}

nts::Tristate nts::AComponent::getLink(std::size_t pin) const {
    auto it = _links.find(pin);
    if (it == _links.end() || !it->second.first) {
        return nts::Undefined;
    }
    return it->second.first->compute(it->second.second);
}
