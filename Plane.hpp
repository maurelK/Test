#ifndef PLANE_HPP
#define PLANE_HPP

#include "Color.hpp"
#include "IPrimitive.hpp"
#include <string>

class Plane : public IPrimitive {
public:
    Plane(const std::string& axis, float position, const Color& color);

    const std::string& getAxis() const { return axis_; }
    float getPosition() const { return position_; }
    const Color& getColor() const { return color_; }

    bool intersect(const Ray& ray, HitRecord& hit) const override;

private:
    std::string axis_;
    float position_;
    Color color_;
    Vec3 point;
    Vec3 norm;
};

#endif
