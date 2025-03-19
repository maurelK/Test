#ifndef FACTORY_HPP_
#define FACTORY_HPP_

#include "IComponent.hpp"
#include "InputComponent.hpp"
#include "OutputComponent.hpp"
#include "AndComponent.hpp"
#include "OrComponent.hpp"
#include "XorComponent.hpp"
#include "NorComponent.hpp"
#include "NotComponent.hpp"
#include "NandComponent.hpp"
#include <memory>
#include <unordered_map>
#include <functional>
#include "FalseComponent.hpp"
#include "TrueComponent.hpp"
#include "ClockComponent.hpp"

class Factory {
public:
    static std::unique_ptr<nts::IComponent> createComponent(const std::string &type);
};

#endif
