#include "Plane.hpp"

Plane::Plane(const std::string& axis, float position, const Color& color)
    : axis_(axis), position_(position), color_(color) {}



bool Plane::intersect(const Ray &ray, HitRecord &hit) const {
    float denom;
    float t;
    Vec3 normal;

    // Choix de la normale selon l'axe
    if (axis_ == "X") {
        denom = ray.direction.x;
        if (std::abs(denom) < 1e-6f) return false; // parallèle
        t = (position_ - ray.origin.x) / denom;
        normal = Vec3(1, 0, 0);
    } else if (axis_ == "Y") {
        denom = ray.direction.y;
        if (std::abs(denom) < 1e-6f) return false;
        t = (position_ - ray.origin.y) / denom;
        normal = Vec3(0, 1, 0);
    } else if (axis_ == "Z") {
        denom = ray.direction.z;
        if (std::abs(denom) < 1e-6f) return false;
        t = (position_ - ray.origin.z) / denom;
        normal = Vec3(0, 0, 1);
    } else {
        return false; // axe inconnu
    }

    // Intersection valide uniquement si t > 0
    if (t < 0)
        return false;

    hit.t = t;
    hit.point = ray.at(t);
    hit.normal = normal;
    hit.color = color_;
    hit.hit = true;

    return true;
}
