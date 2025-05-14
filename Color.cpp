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
    int r = setting["r"];
    int g = setting["g"];
    int b = setting["b"];
    return Color(r, g, b);
}
