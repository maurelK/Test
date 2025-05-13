/*
** EPITECH PROJECT, 2025
** gffe
** File description:
** frrttr
*/

#ifndef VECTOR3
#define VECTOR3

class Vector3 {
    public:
        float x, y, z;
    
        Vector3() : x(0), y(0), z(0) {}
        Vector3(float x, float y, float z) : x(x), y(y), z(z) {}
        Vector3(const libconfig::Setting& setting) {
            x = setting["x"];
            y = setting["y"];
            z = setting["z"];
        }
    };
    
#endif    