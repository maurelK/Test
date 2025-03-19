#include "Circuit.hpp"
#include "Factory.hpp"
#include <iostream>
#include <sstream>
#include <map>
#include "ParsingComponent.hpp"
#include <string>
#include <algorithm>


std::string cleanstr(std::string str, char to_remove)
{
    str.erase(std::remove(str.begin(), str.end(), to_remove), str.end());
    return str;
}

void myhell(Circuit &circuit)
{
    std::string command;
    std::map<std::string, nts::InputComponent*> inputs;
    std::map<std::string, nts::ClockComponent*> clocks;

    for (const auto &[name, comp] : circuit.getComponents()) {
        if (auto *input = dynamic_cast<nts::InputComponent *>(comp.get())) {
            inputs[name] = input;
        } else if (auto *clock = dynamic_cast<nts::ClockComponent *>(comp.get())) {
            clocks[name] = clock;
        }
    }

    while (true) {
        std::cout << "> ";
        if (!std::getline(std::cin, command)) {
            break;
        }
        std::stringstream ss(command);
        std::string token;
        ss >> token;

        if (token == "exit") {
            break;
        } else if (token == "simulate") {
            circuit.simulate(1);
        } else if (token == "display") {
            circuit.display();
        } else if (token.find("=") != std::string::npos) {
            std::string inputName = token.substr(0, token.find("="));
            std::string valueStr = token.substr(token.find("=") + 1);

            nts::Tristate newValue = (valueStr == "1") ? nts::True :
                (valueStr == "0") ? nts::False :
                nts::Undefined;

            if (inputs.find(inputName) != inputs.end()) {
                inputs[inputName]->setValue(newValue);
            } else if (clocks.find(inputName) != clocks.end()) {
                clocks[inputName]->setValue(newValue);
            } else {
                continue;
            }
        }
    }
}


int main(int argc, char **argv)
{
    if (argc != 2) {
        return 84;
    }
    Circuit circuit;
    ParsingComponent parser;

    if (parser.readfile(argv[1]) == 84 || parser.elemExtractChipset(circuit) == 84
    || parser.elemExtractLinks(circuit) == 84) {
        return 84;
    }
    myhell(circuit);
    return 0;
}
