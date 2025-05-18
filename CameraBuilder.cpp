#include "CameraBuilder.hpp"
#include "Vec3.hpp"

Camera CameraBuilder::build(const libconfig::Setting &setting) {
    int width = setting["resolution"]["width"];
    int height = setting["resolution"]["height"];
    Vec3 position = parseVec3(setting["position"]);
    Vec3 rotation = parseVec3(setting["rotation"]);
    float fov = static_cast<float>(setting["fieldOfView"]);

    return Camera(width, height, position, rotation, fov);
}
