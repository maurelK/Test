#ifndef PLANE_HPP
#define PLANE_HPP

#include "Color.hpp"
#include "IPrimitive.hpp"
#include <string>

class Plane : public IPrimitive {
public:
    Plane(const std::string& axis, float position, const Color& color);
    // Add other necessary methods and members here

private:
    std::string axis_;
    float position_;
    Color color_;
};

#endif // PLANE_HPP
