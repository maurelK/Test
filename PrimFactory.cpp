#include "PrimFactory.hpp"
#include "Sphere.hpp"
#include "Plane.hpp"
#include "Vec3.hpp"
#include "Color.hpp"

extern Color parseColor(const libconfig::Setting &setting);

std::unique_ptr<IPrimitive> PrimFactory::create(const std::string& type, const libconfig::Setting& s) {
    if (type == "sphere") {
        const libconfig::Setting &center = s["center"];
        const libconfig::Setting &color = s["color"];
        int radius = 0;
        if (s.exists("radius")) {
            if (s["radius"].getType() == libconfig::Setting::TypeInt) {
                radius = s["radius"];
            } else {
                throw std::runtime_error("Invalid type for radius");
            }
        } else {
            throw std::runtime_error("Missing radius");
        }

        return std::make_unique<Sphere>(
            parseVec3(center),
            radius,
            parseColor(color)
        );
    }

    if (type == "plane") {
        const libconfig::Setting &color = s["color"];
        std::string axis;
        int position;

        if (s.exists("axis") && s["axis"].getType() == libconfig::Setting::TypeString) {
            axis = (const char*)s["axis"];
        } else {
            throw std::runtime_error("Invalid or missing axis");
        }

        if (s.exists("position") && s["position"].getType() == libconfig::Setting::TypeInt) {
            position = s["position"];
        } else {
            throw std::runtime_error("Invalid or missing position");
        }

        return std::make_unique<Plane>(
            axis, position,
            parseColor(color)
        );
    }

    throw std::runtime_error("Unknown primitive type: " + type);
}
