#include "Scene.hpp"

void Scene::setCamera(const Camera& cam) {
    _camera = cam;
}

const Camera& Scene::getCamera() const {
    return _camera;
}

void Scene::addPrimitive(std::unique_ptr<IPrimitive> prim) {
    _primitives.push_back(std::move(prim));
}

const std::vector<std::unique_ptr<IPrimitive>>& Scene::getPrimitives() const {
    return _primitives;
}

//void Scene::addLight(std::unique_ptr<ILight> light) {
//    _lights.push_back(std::move(light));
//}
//
//const std::vector<std::unique_ptr<ILight>>& Scene::getLights() const {
//    return _lights;
//}
