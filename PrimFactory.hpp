/*
** EPITECH PROJECT, 2025
** Loader
** File description:
** Load our scene class
*/
#ifndef PRIMFACTORY
    #define PRIMFACTORY
    #include <iostream>
    #include <string>
    #include <fstream>
    #include <sstream>
    #include <libconfig.h++>
    #include <algorithm>
    #include <stdexcept>
    #include <memory>
    #include "IPrimitive.hpp"
    #include "Vector3.hpp"
    #include "Color.hpp"
class PrimFactory {
private:

public:
    static std::unique_ptr<IPrimitive> create(const std::string& type, const libconfig::Setting& setting);
};
#endif