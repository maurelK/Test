#include "Ray.hpp"
#include "Camera.hpp"
#include "RayGenerator.hpp"
#include <cmath>
#include <iostream>
#include "Vec3.hpp"

RayGenerator::RayGenerator(const Camera& cam, int imageWidth, int imageHeight)
    : camera(cam), width(imageWidth), height(imageHeight) {

    }
//

Vec3 rotate(const Vec3& v, const Vec3& rotDeg) {
    float pitch = rotDeg.x * M_PI / 180.0f; ;
    float yaw   = rotDeg.y * M_PI / 180.0f; ;
    float roll  = rotDeg.z * M_PI / 180.0f; ;

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



//    Ray RayGenerator::generateRay(int x, int y) const {
//    float aspectRatio = static_cast<float>(width) / height;
//    float fovScale = tan((camera.fieldOfView * 0.5f) * M_PI / 180.0f);
//
//    float px = (2 * ((x + 0.5f) / width) - 1) * aspectRatio * fovScale;
//    float py = (1 - 2 * ((y + 0.5f) / height)) * fovScale;
//
//    Vec3 forward = (Vec3(0, 0, 0) - camera.position).normalized();
//    Vec3 right = forward.cross(Vec3(1, 0, 0)).normalized();
//    Vec3 upC = right.cross(forward).normalized();
//    Vec3 dir = (forward + right * px + upC * py).normalized();
//    return Ray(camera.position, dir);
//}
//

Ray RayGenerator::generateRay(int x, int y) const {
    float aspectRatio = static_cast<float>(width) / height;
    float fovScale = tan((camera.fieldOfView * 0.5f) * M_PI / 180.0f);

    float px = (2 * ((x + 0.5f) / width) - 1) * aspectRatio * fovScale;
    float py = (1 - 2 * ((y + 0.5f) / height)) * fovScale;

    // Repère caméra
    Vec3 forward = rotate(Vec3(0, 0, 1), camera.rotation);  // direction vers l'avant
    Vec3 worldUp = Vec3(0, 1, 0);
    Vec3 right = forward.cross(worldUp).normalized();
    Vec3 up = right.cross(forward).normalized();

    // Rayon
    Vec3 dir = (forward + right * px + up * py).normalized();
    return Ray(camera.position, dir);
}
