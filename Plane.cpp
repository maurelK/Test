#include "Plane.hpp"

Plane::Plane(const std::string& axis, float position, const Color& color)
    : axis_(axis), position_(position), color_(color) {}

bool Plane::intersect(const Ray &ray, HitRecord &hit) const
{
    return false;
}
