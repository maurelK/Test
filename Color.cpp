/*
** EPITECH PROJECT, 2025
** efe
** File description:
** effe
*/

#include "Color.hpp"
#include <libconfig.h++>

Color parseColor(const libconfig::Setting &setting) {
    int r = setting["r"];
    int g = setting["g"];
    int b = setting["b"];
    return Color(r / 255.f, g / 255.f, b / 255.f);

}