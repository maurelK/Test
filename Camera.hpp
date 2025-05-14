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
    #include "Vector3.hpp"

class Camera {
public:
    Camera();
    Camera(int width, int height, const Vector3& position, const Vector3& rotation, float fieldOfView);
    int width;
    int height;
    Vector3 position;
    Vector3 rotation;
    float fieldOfView;
};

#endif