/*
** EPITECH PROJECT, 2025
** Loader
** File description:
** Load our scene class
*/
#ifndef LOADSCENE
    #define LOADSCENE
    #include <iostream>
    #include <string>
    #include <libconfig.h++>
    #include <stdexcept>
    #include "Camera.hpp"
    #include "Color.hpp"
    #include "Scene.hpp"
    #include "Vector3.hpp"
    #include "PrimFactory.hpp"

class Scene {
private:

public:
    void load_scene(const std::string path);
};
#endif