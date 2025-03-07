/*
** EPITECH PROJECT, 2025
** erre
** File description:
** erre
*/

#include "Circuit.hpp"
#include "Input.hpp" 
#include <iostream>
#include <string>

nts::Tristate strToTristate(const std::string &value) {
    if (value == "1")
        return nts::True;
    if (value == "0")
        return nts::False;
        
    return nts::Undefined;
}

int main(int argc, char **argv) {
    if (argc != 2) {
        std::cerr << "Usage: ./nanotekspice file.nts" << std::endl;
        return 84;
    }

    Circuit circuit;
    circuit.loadFromFile(argv[1]);

    std::string command;
    std::cout << "> ";
    size_t tick = 0;
    while (std::getline(std::cin, command)) {
        if (command == "exit")
            break;
         if (command == "simulate") {
            circuit.simulate();
            tick++;
        } else if (command == "display") {
            circuit.display(tick);
        } else if (command.find('=') != std::string::npos) {
            size_t pos = command.find('=');
            std::string inputName = command.substr(0, pos);
            std::string value = command.substr(pos + 1);

            nts::IComponent *comp = circuit.getComponent(inputName);
            if (comp) {
                Input *input = dynamic_cast<Input *>(comp);
                if (input)
                    input->setValue(strToTristate(value));
                else
                    std::cerr << "Error: '" << inputName << "' is not an input component" << std::endl;
            } else {
                std::cerr << "Error: Unknown component '" << inputName << "'" << std::endl;
            }
        } else {
            std::cerr << "Error: Unknown command '" << command << "'" << std::endl;
        }
        std::cout << "> ";
    }

    return 0;
}
