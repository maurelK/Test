/*
** EPITECH PROJECT, 2025
** efe
** File description:
** effe
*/

#include "Color.hpp"
#include <libconfig.h++>

Color parseColor(const libconfig::Setting &setting)
{
    float r = setting["r"];
    float g = setting["g"];
    float b = setting["b"];
    return Color(r, g, b);
}
