#include "Camera.hpp"

Camera::Camera() : width(0), height(0), position(0, 0, 0), rotation(0, 0, 0), fieldOfView(0) {}

Camera::Camera(int width_, int height_, const Vector3& pos, const Vector3& rot, float fov)
    : width(width_), height(height_), position(pos), rotation(rot), fieldOfView(fov) {}
