/*
** EPITECH PROJECT, 2025
** ff
** File description:
** fef
*/

#include "Vector3.hpp"
#include <libconfig.h++>

Vector3 parseVec3(const libconfig::Setting &setting)
{
    float x = setting["x"];
    float y = setting["y"];
    float z = setting["z"];
    return Vector3(x, y, z);
}