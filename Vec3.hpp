/*
** EPITECH PROJECT, 2025
** gffe
** File description:
** frrttr
*/

#pragma once
#include <libconfig.h++>
#include <stdexcept>
struct Vec3 {
    int x, y, z;
    Vec3(int x_, int y_, int z_) : x(x_), y(y_), z(z_) {}

    static int getInt(const libconfig::Setting& setting, const char* key) {
        if (!setting.exists(key)) {
            throw std::runtime_error(std::string("Missing key: ") + key);
        }
        const libconfig::Setting& val = setting[key];
        if (val.getType() != libconfig::Setting::TypeInt) {
            throw std::runtime_error(std::string("Invalid type for key: ") + key);
        }
        return val;
    }
};

Vec3 parseVec3(const libconfig::Setting &setting);
