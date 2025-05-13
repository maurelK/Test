#include "PrimFactory.hpp"
#include "Sphere.hpp"
#include "Plane.hpp"

std::unique_ptr<IPrimitive> PrimFactory::create(const std::string& type, const libconfig::Setting& s) {
    if (type == "sphere") {
        const libconfig::Setting &center = s["center"];
        const libconfig::Setting &color = s["color"];
        return std::make_unique<Sphere>(
            Vector3(center["x"], center["y"], center["z"]),
            s["radius"],
            Color(color["r"], color["g"], color["b"])
        );
    }

    if (type == "plane") {
        const libconfig::Setting &color = s["color"];
        return std::make_unique<Plane>(
            s["axis"], s["position"],
            Color(color["r"], color["g"], color["b"])
        );
    }

    throw std::runtime_error("Unknown primitive type: " + type);
}
