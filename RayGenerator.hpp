#ifndef RAYGENERATOR_HPP
#define RAYGENERATOR_HPP

#include "Ray.hpp"
#include "Camera.hpp"

class RayGenerator {
public:
    RayGenerator(const Camera& cam, int imageWidth, int imageHeight);

    Ray generateRay(int x, int y) const;

private:
    Camera camera;
    int width;
    int height;
};

#endif
