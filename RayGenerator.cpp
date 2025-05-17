#include "Ray.hpp"
#include "Camera.hpp"
#include "RayGenerator.hpp"
#include <cmath>

RayGenerator::RayGenerator(const Camera& cam, int imageWidth, int imageHeight)
    : camera(cam), width(imageWidth), height(imageHeight) {}

//Ray RayGenerator::generateRay(int x, int y) const {
//    float aspectRatio = static_cast<float>(width) / height;
//    float fovScale = tan((camera.fieldOfView * 0.5f) * M_PI / 180.0f);
//
//    float px = (2 * ((x + 0.5f) / width) - 1) * aspectRatio * fovScale;
//    float py = (1 - 2 * ((y + 0.5f) / height)) * fovScale;
//
//    Vec3 dir(px, py, 1);
//    return Ray(camera.position, dir);
//
//}

Vec3 rotate(const Vec3& v, const Vec3& rotDeg) {
    float pitch = rotDeg.x * M_PI / 180.0f;
    float yaw   = rotDeg.y * M_PI / 180.0f;
    float roll  = rotDeg.z * M_PI / 180.0f;

    Vec3 res = v;

    // Rotation X (pitch)
    res = Vec3(
        res.x,
        res.y * cos(pitch) - res.z * sin(pitch),
        res.y * sin(pitch) + res.z * cos(pitch)
    );

    // Rotation Y (yaw)
    res = Vec3(
        res.x * cos(yaw) + res.z * sin(yaw),
        res.y,
        -res.x * sin(yaw) + res.z * cos(yaw)
    );

    // Rotation Z (roll)
    res = Vec3(
        res.x * cos(roll) - res.y * sin(roll),
        res.x * sin(roll) + res.y * cos(roll),
        res.z
    );

    return res;
}

Ray RayGenerator::generateRay(int x, int y) const {
    float aspectRatio = static_cast<float>(width) / height;
    float fovScale = tan((camera.fieldOfView * 0.5f) * M_PI / 180.0f);

    float px = (2 * ((x + 0.5f) / width) - 1) * aspectRatio * fovScale;
    float py = (1 - 2 * ((y + 0.5f) / height)) * fovScale;

    Vec3 dir(px, py, 1);
    dir = rotate(dir, camera.rotation);  // Ajout ici
    return Ray(camera.position, dir.normalized());
}
