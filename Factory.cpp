#include "Factory.hpp"

std::unique_ptr<nts::IComponent> Factory::createComponent(const std::string &type) {
    static const std::unordered_map<std::string, std::function<std::unique_ptr<nts::IComponent>()>> creators = {
        {"input", []() { return std::make_unique<nts::InputComponent>(); }},
        {"output", []() { return std::make_unique<nts::OutputComponent>(); }},
        {"and", []() { return std::make_unique<nts::AndComponent>(); }},
        {"or", []() { return std::make_unique<nts::OrComponent>(); }},
        {"xor", []() { return std::make_unique<nts::XorComponent>(); }},
        {"nor", []() { return std::make_unique<nts::NorComponent>(); }},
        {"not", []() { return std::make_unique<nts::NotComponent>(); }},
        {"nand", []() { return std::make_unique<nts::NandComponent>(); }},
        {"4081", []() { return std::make_unique<nts::AndComponent>(); }},
        {"4001", []() { return std::make_unique<nts::NorComponent>(); }},
        {"4030", []() { return std::make_unique<nts::XorComponent>(); }},
        {"4071", []() { return std::make_unique<nts::OrComponent>(); }},
        {"4011", []() { return std::make_unique<nts::NandComponent>(); }},
        {"4069", []() { return std::make_unique<nts::NotComponent>(); }},
        {"false", []() { return std::make_unique<nts::FalseComponent>(); }},
        {"true", []() { return std::make_unique<nts::TrueComponent>(); }},
    };

    auto it = creators.find(type);
    return (it != creators.end()) ? it->second() : nullptr;
}
