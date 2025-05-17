#ifndef SPHERE_HPP
#define SPHERE_HPP

#include "Vec3.hpp"
#include "Color.hpp"
#include "IPrimitive.hpp"

class Sphere : public IPrimitive {
public:
    Sphere(const Vec3& center, float radius, const Color& color);

    const Vec3& getCenter() const { return center_; }
    float getRadius() const { return radius_; }
    const Color& getColor() const { return color_; }

    bool intersect(const Ray& ray, HitRecord& hit) const override;

private:
    Vec3 center_;
    float radius_;
    Color color_;
};

#endif
