#ifndef FACTORY_HPP_
#define FACTORY_HPP_

#include "IComponent.hpp"
#include "InputComponent.hpp"
#include "OutputComponent.hpp"
#include "AndComponent.hpp"
#include <memory>
#include <unordered_map>
#include <functional>

class Factory {
public:
    static std::unique_ptr<nts::IComponent> createComponent(const std::string &type);
};

#endif
