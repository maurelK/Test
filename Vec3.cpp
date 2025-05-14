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

Vec3 parseVec3(const libconfig::Setting &setting)
{
    int x = Vec3::getInt(setting, "x");
    int y = Vec3::getInt(setting, "y");
    int z = Vec3::getInt(setting, "z");
    return Vec3(x, y, z);
}
