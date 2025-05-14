#include "Scene.hpp"
#include "PrimFactory.hpp"
#include "Vec3.hpp"

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

void Scene::addLight(std::unique_ptr<ILight> light) {
    _lights.push_back(std::move(light));
}

const std::vector<std::unique_ptr<ILight>>& Scene::getLights() const {
    return _lights;
}

void Scene::load_scene(const std::string& path) {
    libconfig::Config cfg;

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

    try {
        const auto& cam = cfg.lookup("camera");
        std::cout << "cam[\"resolution\"][\"width\"]: " << (int)cam["resolution"]["width"] << std::endl;
        std::cout << "cam[\"resolution\"][\"height\"]: " << (int)cam["resolution"]["height"] << std::endl;
        this->camera.width = (int)cam["resolution"]["width"];
        this->camera.height = (int)cam["resolution"]["height"];
        std::cout << "cam[\"position\"] type: " << cam["position"].getType() << std::endl;
        std::cout << "cam[\"rotation\"] type: " << cam["rotation"].getType() << std::endl;
        Vec3 position = parseVec3(cam["position"]);
        Vec3 rotation = parseVec3(cam["rotation"]);
        std::cout << "cam[\"fieldOfView\"] type: " << cam["fieldOfView"].getType() << std::endl;
        this->camera.fieldOfView = static_cast<float>(cam["fieldOfView"]);
    } catch (const libconfig::SettingTypeException &ex) {
        std::cerr << "SettingTypeException in camera section: " << ex.what() << std::endl;
        throw;
    }

    try {
        const auto& prims = cfg.lookup("primitives");
            
        if (prims.exists("spheres")) {
            const auto& spheres = prims["spheres"];
            for (int i = 0; i < spheres.getLength(); ++i) {
                auto sphere = PrimFactory::create("sphere", spheres[i]);
                this->addPrimitive(std::move(sphere));
            }
        }

        if (prims.exists("planes")) {
            const auto& planes = prims["planes"];
            for (int i = 0; i < planes.getLength(); ++i) {
                auto plane = PrimFactory::create("plane", planes[i]);
                this->addPrimitive(std::move(plane));
            }
        }
    } catch (const libconfig::SettingTypeException &ex) {
        std::cerr << "SettingTypeException in primitives section: " << ex.what() << std::endl;
        throw;
    }
    try {
        const auto& lights = cfg.lookup("lights");

        if (lights.exists("ambient")) {
            float ambientIntensity = static_cast<float>(lights["ambient"]);
            this->addLight(std::make_unique<AmbientLight>(ambientIntensity));
        }
        if (lights.exists("diffuse")) {
            float diffuseIntensity = static_cast<float>(lights["diffuse"]);
            this->addLight(std::make_unique<AmbientLight>(diffuseIntensity));
        }
        if (lights.exists("point")) {
            const auto& pointLights = lights["point"];
            for (int i = 0; i < pointLights.getLength(); ++i) {
                Vec3 pos = parseVec3(pointLights[i]["position"]);
                this->addLight(std::make_unique<PointLight>(pos));
            }
        }
        if (lights.exists("directional")) {
            const auto& directionalLights = lights["directional"];
            for (int i = 0; i < directionalLights.getLength(); ++i) {
                Vec3 dir = parseVec3(directionalLights[i]["direction"]);
                this->addLight(std::make_unique<DirectionalLight>(dir));
            }
        }
    } catch (const libconfig::SettingTypeException &ex) {
        std::cerr << "SettingTypeException in lights section: " << ex.what() << std::endl;
        throw;
    }
}
