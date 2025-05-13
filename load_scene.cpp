
#include "load_scene.hpp"

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
    scene.camera.resolution = {
        cam["resolution"]["width"],
        cam["resolution"]["height"],
    };
    scene.camera.position = {
        cam["position"]["x"], cam["position"]["y"], cam["position"]["z"]
    };
    scene.camera.rotation = {
        cam["rotation"]["x"], cam["rotation"]["y"], cam["rotation"]["z"]
    };
    scene.fieldOfValue = cam["fieldOfValue"];

    const auto& prims = cfg.lookup("primitives");
    for (int i = 0; i < prims.getLength(); i++) {
        const auto& prim = prims[i];
        std::string k_type = p[k_type];
        auto primitive = PrimFactory::create(k_type, p);
        scene.addPrimitive(std::move(primitive));
    }
    return scene;
}
