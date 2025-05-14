/*
** EPITECH PROJECT, 2025
** Loader
** File description:
** Load our scene class
*/
#ifndef CAMERA
    #define CAMERA
    #include <libconfig.h++>
    #include <vector>
    #include <memory>
    #include "Vec3.hpp"

class Camera {
public:
    Camera();
    Camera(int width, int height, const Vec3& position, const Vec3& rotation, float fieldOfView);
    int width;
    int height;
    Vec3 position;
    Vec3 rotation;
    float fieldOfView;
};

#endif