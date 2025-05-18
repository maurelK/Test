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

    Vec3 operator-(const Vec3& other) const {
        return Vec3(x - other.x, y - other.y, z - other.z);
    }

    Vec3 operator+(const Vec3& other) const {
        return Vec3(x + other.x, y + other.y, z + other.z);
    }

    Vec3 operator*(const float n) const {
        return Vec3(n * x, n * y, n * z);
    }

    Vec3 operator*(const int n) const {
        return Vec3(n * x, n * y, n * z);
    }

    Vec3 operator*(const Vec3& other) const {
        return Vec3(x * other.x, y * other.y, z * other.z);
    }


    Vec3 operator/(const Vec3& other) const {
        return Vec3(x / other.x, y / other.y, z / other.z);
    }

    double dot(const Vec3& other) const {
        return x * other.x + y * other.y + z * other.z;
    }

    Vec3 cross(const Vec3& v) const {
        return Vec3(
            y * v.z - z * v.y,
            z * v.x - x * v.z,
            x * v.y - y * v.x
        );
    }

    float length() const {
        return sqrt( x * x + y * y + z * z);
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
