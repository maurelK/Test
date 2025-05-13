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
    Camera(int widht, int height, const Vector3& position, const Vector3& rotation, float fieldOfView);
    int _widht;

};
#endif
