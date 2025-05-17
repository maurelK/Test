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
    #include "Vec3.hpp"
    #include "Ray.hpp"
    #include "Color.hpp"

    struct HitRecord {
        float t;
        Vec3 point;
        Vec3 normal;
        Color color;
        bool hit;
    };
    
    class IPrimitive {
    public:
        virtual ~IPrimitive() = default;
        virtual bool intersect(const Ray& ray, HitRecord& hit) const = 0;
    };
    
#endif