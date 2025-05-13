/*
** EPITECH PROJECT, 2025
** Loader
** File description:
** Load our scene class
*/
#ifndef SCENE
    #define SCENE
    #include <libconfig.h++>
    #include <vector>
    #include <memory>
    #include "Camera.hpp"
    #include "Color.hpp"
    #include "IPrimitive.hpp"

class Scene {
public:

    void setCamera(const Camera& cam);
    const Camera& getCamera() const;
    void addPrimitive(std::unique_ptr<IPrimitive> prim);
    const std::vector<std::unique_ptr<IPrimitive>>& getPrimitives() const;
    //void addLight(std::unique_ptr<ILight> light);
    //const std::vector<std::unique_ptr<ILight>>& getLights() const;

private:
    Camera camera;
    std::vector<std::unique_ptr<IPrimitive>> _primitives;
    //std::vector<std::unique_ptr<ILight>> _lights; // Optional
};

#endif
