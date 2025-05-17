#ifndef SCENE_HPP
#define SCENE_HPP

#include <string>
#include <vector>
#include <memory>
#include "IPrimitive.hpp"
#include "Camera.hpp"
#include "ILight.hpp"
#include <memory>
#include "Vec3.hpp"
#include "Ray.hpp"
#include "RayGenerator.hpp"


class Scene {
public:
    void load_scene(const std::string& path);
    void setCamera(const Camera& cam);
    const Camera& getCamera() const;
    void addPrimitive(std::unique_ptr<IPrimitive> prim);
    const std::vector<std::unique_ptr<IPrimitive>>& getPrimitives() const;

    void addLight(std::unique_ptr<ILight> light);
    const std::vector<std::unique_ptr<ILight>>& getLights() const;
    Color trace(const Ray& ray) const;
    void render(int width, int height, const RayGenerator& rayGen, const std::string& filename) const;

    
private:
    Camera camera;
    std::vector<std::unique_ptr<IPrimitive>> _primitives;
    std::vector<std::unique_ptr<ILight>> _lights;
};

#endif // SCENE_HPP
