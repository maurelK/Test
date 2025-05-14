/*
** EPITECH PROJECT, 2025
** ff
** File description:
** fef
*/

#include "Vec3.hpp"
#include <libconfig.h++>

#include "Vec3.hpp"

#include "Vec3.hpp"

#include "Vec3.hpp"

Vec3 parseVec3(const libconfig::Setting &setting) {
    float x = Vec3::getFloat(setting, "x");
    float y = Vec3::getFloat(setting, "y");
    float z = Vec3::getFloat(setting, "z");
    return Vec3(x, y, z);
}