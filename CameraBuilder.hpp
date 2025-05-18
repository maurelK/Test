/*
** EPITECH PROJECT, 2025
** Loader
** File description:
** Load our scene class
*/
#ifndef CAMERABUILDER
    #define CAMERABUILDER
    #include <libconfig.h++>
    #include "Camera.hpp"

class CameraBuilder {
public:
    static Camera build(const libconfig::Setting &setting);
};

#endif