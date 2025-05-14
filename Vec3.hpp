/*
** EPITECH PROJECT, 2025
** gffe
** File description:
** frrttr
*/

#pragma once
#include <libconfig.h++>
#include <stdexcept>

#pragma once
#include <libconfig.h++>
#include <stdexcept>

struct Vec3 {
    float x, y, z;
    Vec3(float x_ = 0, float y_ = 0, float z_ = 0) : x(x_), y(y_), z(z_) {}

    static float getFloat(const libconfig::Setting& setting, const char* key) {
        if (!setting.exists(key)) {
            throw std::runtime_error(std::string("Missing key: ") + key);
        }
        const libconfig::Setting& val = setting[key];
        if (val.getType() != libconfig::Setting::TypeFloat && 
            val.getType() != libconfig::Setting::TypeInt) {
            throw std::runtime_error(std::string("Invalid type for key: ") + key);
        }
        return val.getType() == libconfig::Setting::TypeFloat ? 
               static_cast<float>(val) : static_cast<float>((int)val);
    }
};

Vec3 parseVec3(const libconfig::Setting &setting);