#ifndef SPHERE_HPP
#define SPHERE_HPP

#include "Vector3.hpp"
#include "Color.hpp"
#include "IPrimitive.hpp"

class Sphere : public IPrimitive {
public:
    Sphere(const Vector3& center, float radius, const Color& color);
    // Add other necessary methods and members here

private:
    Vector3 center_;
    float radius_;
    Color color_;
};

#endif // SPHERE_HPP
