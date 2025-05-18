#pragma once

#include "Vec3.hpp"
#include <cmath>

struct Ray {
    Vec3 origin;
    Vec3 direction;
    Ray(const Vec3& o, const Vec3& d)
        : origin(o), direction(d.normalized()) {}

    Vec3 at(float t) const {
        return Vec3(
            origin.x + t * direction.x,
            origin.y + t * direction.y,
            origin.z + t * direction.z
        );
    }
};
