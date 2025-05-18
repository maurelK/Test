#include "Sphere.hpp"
#include "Ray.hpp"
#include "Ray.hpp"
#include <cmath>

Sphere::Sphere(const Vec3& center, float radius, const Color& color)
    : center_(center), radius_(radius), color_(color) {}

bool Sphere::intersect(const Ray& ray, HitRecord& hit) const {
    Vec3 oc = center_ - ray.origin;
    float a = ray.direction.dot(ray.direction);
    float b = -2.0f * oc.dot(ray.direction);
    float c = oc.dot(oc) - radius_ * radius_;
    float discriminant = b * b - 4 * a * c;

    if (discriminant < 0) {
        return false;
    }

    float sqrt_disc = std::sqrt(discriminant);
    float t1 = (-b - sqrt_disc) / (2 * a);
    float t2 = (-b + sqrt_disc) / (2 * a);

    float t = (t1 >= 0) ? t1 : ((t2 >= 0) ? t2 : -1);
    if (t < 0)
        return false;

    hit.t = t;
    hit.point = ray.at(t);
    hit.normal = (hit.point - center_).normalized();
    hit.color = color_;
    hit.hit = true;

    return true;
}
