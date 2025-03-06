#include "Factory.hpp"

std::unique_ptr<nts::IComponent> Factory::createComponent(const std::string &type) {
    static const std::unordered_map<std::string, std::function<std::unique_ptr<nts::IComponent>()>> creators = {
        {"input", []() { return std::make_unique<nts::InputComponent>(); }},
        {"output", []() { return std::make_unique<nts::OutputComponent>(); }},
        {"4081", []() { return std::make_unique<nts::AndComponent>(); }},
        //{"false", []() { return std::make_unique<nts::FalseComponent>(); }},
        //{"true", []() { return std::make_unique<nts::TrueComponent>(); }},
    };

    auto it = creators.find(type);
    return (it != creators.end()) ? it->second() : nullptr;
}
