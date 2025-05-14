#include "PrimFactory.hpp"
#include "Sphere.hpp"
#include "Plane.hpp"
#include "Vec3.hpp"
#include "Color.hpp"

extern Color parseColor(const libconfig::Setting &setting);

std::unique_ptr<IPrimitive> PrimFactory::create(const std::string& type, const libconfig::Setting& s) {
    if (type == "sphere") {
        const libconfig::Setting &center = s["center"];
        int radius = s["radius"];  // Read as integer
        const libconfig::Setting &color = s["color"];
        return std::make_unique<Sphere>(
            parseVec3(center),
            static_cast<float>(radius),  // Convert to float
            parseColor(color)
        );
    }

    if (type == "plane") {
        std::string axis = s["axis"];
        int position = s["position"];  // Read as integer
        const libconfig::Setting &color = s["color"];
        return std::make_unique<Plane>(
            axis,
            static_cast<float>(position),  // Convert to float
            parseColor(color)
        );
    }

    throw std::runtime_error("Unknown primitive type: " + type);
}