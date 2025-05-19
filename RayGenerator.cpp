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

    Vec3 forward = (Vec3(0, 0, 0) - camera.position).normalized();
    Vec3 right = forward.cross(Vec3(1, 0, 0)).normalized();
    Vec3 upC = right.cross(forward).normalized();
    Vec3 dir = (forward + right * px + upC * py).normalized();
    return Ray(camera.position, dir);
}


/*Vec3 rotate(const Vec3& v, const Vec3& rotDeg)
{
    float yaw = rotDeg.y * M_PI / 180.0f;
    float pitch = rotDeg.x * M_PI / 180.0f;
    float roll = rotDeg.z * M_PI / 180.0f;

    float cy = cos(yaw);
    float sy = sin(yaw);
    float cp = cos(pitch);
    float sp = sin(pitch);
    float cr = cos(roll);
    float sr = sin(roll);

    Vec3 rotated;
    rotated.x = v.x * cy - v.z * sy;
    rotated.z = v.x * sy + v.z * cy;
    rotated.y = v.y;
    float y = rotated.y;
    rotated.y = y * cp - rotated.z * sp;
    rotated.z = y * sp + rotated.z * cp;
    float tx = rotated.x;
    rotated.x = tx * cr - rotated.y * sr;
    rotated.y = tx * sr + rotated.y * cr;
    return rotated;
}

Ray RayGenerator::generateRay(int x, int y) const
{
    float aspectRatio = static_cast<float>(width) / height;
    float tanFov = tan(camera.fieldOfView * 0.5f * M_PI / 180.0f);
    float px = (2.0f * (x + 0.5f) / width - 1.0f) * aspectRatio * tanFov;
    float py = (1.0f - 2.0f * (y + 0.5f) / height) * tanFov;
    Vec3 forward = rotate(Vec3(0, 0, -1), camera.rotation);
    Vec3 right = rotate(Vec3(1, 0, 0), camera.rotation);
    Vec3 up = rotate(Vec3(0, 1, 0), camera.rotation);
    Vec3 rayDir = (forward + right * px + up * py).normalized();
    return Ray(camera.position, rayDir);
}*/
