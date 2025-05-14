#include "Scene.hpp"

void Scene::setCamera(const Camera& cam) {
    camera = cam;
}

const Camera& Scene::getCamera() const {
    return camera;
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


void Scene::load_scene(const std::string path)
{
    libconfig::Config cfg;
    Scene scene;

    try {
        cfg.readFile(path.c_str());
    } catch (const libconfig::FileIOException &fioex) {
        std::cerr << "I/O error while reading file." << std::endl;
        throw;
    } catch (const libconfig::ParseException &pex) {
        std::cerr << "Parse error at " << pex.getFile() << ":" << pex.getLine()
                  << " - " << pex.getError() << std::endl;
        throw;
    }

    const auto& cam = cfg.lookup("camera");
    this->camera.width = cam["resolution"]["width"];
    this->camera.height = cam["resolution"]["height"];
    this->camera.position = Vector3(cam["position"]["x"], cam["position"]["y"], cam["position"]["z"]);
    this->camera.rotation = Vector3(cam["rotation"]["x"], cam["rotation"]["y"], cam["rotation"]["z"]);
    this->camera.fieldOfView = cam["fieldOfView"];


    const auto& prims = cfg.lookup("primitives");
    for (int i = 0; i < prims.getLength(); i++) {
        const auto& prim = prims[i];
        std::string k_type = prims[k_type];
        auto primitive = PrimFactory::create(k_type, prims);
        scene.addPrimitive(std::move(primitive));
    }
}

int main(int ac, char **av)
{
    if (ac != 2) {
        return 84;
    }
    Scene scene;
    scene.load_scene(av[1]);
}