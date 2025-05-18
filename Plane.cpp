#include "Plane.hpp"

Plane::Plane(const std::string& axis, float position, const Color& color)
    : axis_(axis), position_(position), color_(color) {
        std::cout << color.r << "  " << color.g << "  " << color.b << "\n";
        if (axis == "X") {
            point = (position, 0, 0);
            norm = (1, 0, 0);
        } else if (axis == "Y") {
            point = (0, position, 0);
            norm = (0, 1, 0);
        } else if (axis == "Z") {
            point = (0, 0, position);
            norm = (0, 0, 1);
        } else {
            throw std::runtime_error("Cannot normalize zero vector");
        }
    }

bool Plane::intersect(const Ray &ray, HitRecord &hit) const
{
    double d = norm.dot(ray.direction);
    if (fabs(d) > 1e-6) {
        Vec3 diff = point - ray.origin;
        hit.t = diff.dot(norm) / d;
        hit.hit = hit.t >= 0;
        // std::cout << (hit.hit ? "Hit Somthing" : "");
        hit.color = (hit.hit) ? color_ : Color(0, 0, 0);
        hit.color = (hit.hit) ? color_ : Color(0, 0, 0);
        hit.normal = norm;
        hit.point = ray.origin + ray.direction * hit.t;
        return hit.hit;
    }
    return false;
}
