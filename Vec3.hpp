/*
** EPITECH PROJECT, 2025
** gffe
** File description:
** frrttr
*/
#pragma once
#include <cmath>
#include <libconfig.h++>
#include <stdexcept>

struct Vec3 {
    float x, y, z;

    Vec3(float x_ = 0, float y_ = 0, float z_ = 0) : x(x_), y(y_), z(z_) {}

    Vec3 normalized() const {
        float norm = std::sqrt(x * x + y * y + z * z);
        if (norm == 0.0f)
            throw std::runtime_error("Cannot normalize zero vector");
        return Vec3(x / norm, y / norm, z / norm);
    }
    Vec3 operator-() const {
        return Vec3(-x, -y, -z);
    }
    Vec3 operator-(const Vec3& other) const {
        return Vec3(x - other.x, y - other.y, z - other.z);
    }

    // Multiplication Vec3 * float
Vec3 operator*(float scalar) const {
    return Vec3(x * scalar, y * scalar, z * scalar);
}

// Division Vec3 / float (utile aussi)
Vec3 operator/(float scalar) const {
    if (scalar == 0.0f)
        throw std::runtime_error("Division by zero in Vec3");
    return Vec3(x / scalar, y / scalar, z / scalar);
}

    float dot(const Vec3& other) const {
        return x * other.x + y * other.y + z * other.z;
    }

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
