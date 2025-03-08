#include "Circuit.hpp"
#include "Factory.hpp"
#include <iostream>
#include <sstream>
#include <map>
#include "ParsingComponent.hpp"
#include <string>
#include <algorithm>


std::string cleanstr(std::string str, char to_remove) {
    str.erase(std::remove(str.begin(), str.end(), to_remove), str.end());
    return str;
}

void runShell(Circuit &circuit) {
    std::string command;
    std::map<std::string, nts::InputComponent*> inputs;

    for (const auto &[name, comp] : circuit.getComponents()) {
        if (auto *input = dynamic_cast<nts::InputComponent *>(comp.get())) {
            inputs[name] = input;
        }
    }
    while (true) {
        std::cout << "> ";
        std::getline(std::cin, command);
        command = cleanstr(command, ' ');
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

            if (inputs.find(inputName) != inputs.end()) {
                nts::Tristate newValue = (valueStr == "1") ? nts::True :
                                         (valueStr == "0") ? nts::False :
                                                              nts::Undefined;
                inputs[inputName]->setValue(newValue);
            } else {
                continue;
            }
        } else {
            continue;
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

    if (parser.readfile(argv[1]) == 84 || parser.elemExtractChipset(circuit) == 84 || parser.elemExtractLinks(circuit) == 84) {
        return 84;
    }
    runShell(circuit);
    return 0;
}
