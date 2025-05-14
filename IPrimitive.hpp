/*
** EPITECH PROJECT, 2025
** Loader
** File description:
** Load our scene class
*/
#ifndef IPRIMITIVE
    #define IPRIMITIVE
    #include <iostream>
    #include <string>
    #include <fstream>
    #include <sstream>
    #include <libconfig.h++>
    #include <algorithm>
    #include <stdexcept>
    #include <memory>
    #include "Color.hpp"

class IPrimitive {
public:
    virtual ~IPrimitive() = default;
    //virtual bool intersect(const Ray& ray, float& t) const = 0;
    //virtual Color getColor() const = 0;
    
};

#endif