#include "Scene.hpp"
#include "PrimFactory.hpp"
#include "Vec3.hpp"
#include "Ray.hpp"
#include "Camera.hpp"
#include "Scene.hpp"
#include <limits>



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
        this->setCamera(Camera(
        cam["resolution"]["width"],
        cam["resolution"]["height"],
        parseVec3(cam["position"]),
        parseVec3(cam["rotation"]),
        static_cast<float>(cam["fieldOfView"])
    ));

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
            this->addLight(std::make_unique<DiffuseLight>(diffuseIntensity));
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

Color Scene::trace(const Ray& ray) const {
    HitRecord closestHit;
    closestHit.t = std::numeric_limits<float>::infinity();
    closestHit.hit = false;

    for (const auto& object : _primitives) {
        HitRecord hit;
        if (object->intersect(ray, hit) && hit.t < closestHit.t) {
            closestHit = hit;
        }
    }

    return closestHit.hit ? closestHit.color : Color(0, 0, 0);
}

void Scene::render(int width, int height, const RayGenerator& rayGen, const std::string& filename) const {
    std::ofstream ppm(filename);
    if (!ppm.is_open()) {
        std::cerr << "Error: Cannot open file " << filename << std::endl;
        return;
    }

    ppm << "P3\n" << width << " " << height << "\n255\n";

    for (int y = 0; y < height; ++y) {
        for (int x = 0; x < width; ++x) {
            Ray ray = rayGen.generateRay(x, y);
            Color color = trace(ray);

            //int r = static_cast<int>(std::clamp(color.r, 0.f, 1.f) * 255);
            //int g = static_cast<int>(std::clamp(color.g, 0.f, 1.f) * 255);
            //int b = static_cast<int>(std::clamp(color.b, 0.f, 1.f) * 255);
            //
            int r = int(255.999 * color.r);
            int g = int(255.999 * color.g);
            int b = int(255.999 * color.b);

            ppm << r << " " << g << " " << b << "\n";
        }
    }

    ppm.close();
    std::cout << "Rendered image written to " << filename << std::endl;
}