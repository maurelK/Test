#ifndef SPHERE_HPP
#define SPHERE_HPP

#include "Vec3.hpp"
#include "Color.hpp"
#include "IPrimitive.hpp"

class Sphere : public IPrimitive {
public:
    Sphere(const Vec3& center, float radius, const Color& color);

private:
    Vec3 center_;
    float radius_;
    Color color_;
};

#endif
