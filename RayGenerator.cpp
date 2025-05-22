#include "Ray.hpp"
#include "Camera.hpp"
#include "RayGenerator.hpp"
#include <cmath>
#include <iostream>
#include "Vec3.hpp"


RayGenerator::RayGenerator(const Camera& cam, int imageWidth, int imageHeight)
    : camera(cam), width(imageWidth), height(imageHeight) {

    }

    Ray RayGenerator::generateRay(int x, int y) const {
    float aspectRatio = static_cast<float>(width) / height;
    float fovScale = tan((camera.fieldOfView * 0.5f) * M_PI / 180.0f);

    float px = (2 * ((x + 0.5f) / width) - 1) * aspectRatio * fovScale;
    float py = (1 - 2 * ((y + 0.5f) / height)) * fovScale;

    Vec3 forward = (Vec3(0, 0, 0) - camera.position).normalized();;
    Vec3 right = forward.cross(Vec3(0, 0, 1)).normalized();
    Vec3 upC = right.cross(forward).normalized();
    Vec3 dir = (forward + right * px + upC * py).normalized();
    return Ray(camera.position, dir);
}
