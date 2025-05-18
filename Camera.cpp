#include "Camera.hpp"

Camera::Camera() : width(800), height(600), position(0, -100, 20), rotation(0, 0, 0), fieldOfView(60) {}

Camera::Camera(int width_, int height_, const Vec3& pos, const Vec3& rot, float fov)
    : width(width_), height(height_), position(pos), rotation(rot), fieldOfView(fov) {}
