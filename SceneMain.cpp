#include "Scene.hpp"
#include "Camera.hpp"
#include "Vec3.hpp"
#include "IPrimitive.hpp"
#include "ILight.hpp"
#include "Sphere.hpp"
#include "Plane.hpp"
#include <iostream>

int main(int argc, char** argv) {
    if (argc != 2) {
        std::cerr << "Usage: " << argv[0] << " <scene_config_file>" << std::endl;
        return 84;
    }
    Scene scene;
    try {
        scene.load_scene(argv[1]);
        const Camera& cam = scene.getCamera();
        RayGenerator rayGen(cam, cam.width, cam.height);
        std::string filename = "render.ppm";
        scene.render(cam.width, cam.height, rayGen, filename);
    } catch (const std::exception& e) {
        return 84;
    }
    return 0;
}




//int main(int argc, char** argv) {
//    if (argc != 2) {
//        return 84;
//    }
//
//    Scene scene;
//
//    try {
//        scene.load_scene(argv[1]);
//        std::cout << "Scene loaded successfully \n" << std::endl;
//
//        // === CAMERA ===
//        const Camera& cam = scene.getCamera();
//        std::cout << "[CAMERA]" << std::endl;
//        std::cout << "Resolution: " << cam.width << " x " << cam.height << std::endl;
//        std::cout << "Position  : (" << cam.position.x << ", " << cam.position.y << ", " << cam.position.z << ")\n";
//        std::cout << "Rotation  : (" << cam.rotation.x << ", " << cam.rotation.y << ", " << cam.rotation.z << ")\n";
//        std::cout << "FOV       : " << cam.fieldOfView << "\n" << std::endl;
//
//        // === PRIMITIVES ===
//        std::cout << "[PRIMITIVES]" << std::endl;
//        int primIdx = 0;
//        for (const auto& prim : scene.getPrimitives()) {
//            std::cout << "- Primitive #" << primIdx++ << ": ";
//
//            if (auto sphere = dynamic_cast<Sphere*>(prim.get())) {
//                std::cout << "Sphere" << std::endl;
//                const Vec3& c = sphere->getCenter();
//                const Color& col = sphere->getColor();
//                std::cout << "    Center : (" << c.x << ", " << c.y << ", " << c.z << ")\n";
//                std::cout << "    Radius : " << sphere->getRadius() << "\n";
//                std::cout << "    Color  : (" << col.r << ", " << col.g << ", " << col.b << ")\n";
//            } else if (auto plane = dynamic_cast<Plane*>(prim.get())) {
//                std::cout << "Plane" << std::endl;
//                const Color& col = plane->getColor();
//                std::cout << "    Axis    : " << plane->getAxis() << "\n";
//                std::cout << "    Position: " << plane->getPosition() << "\n";
//                std::cout << "    Color   : (" << col.r << ", " << col.g << ", " << col.b << ")\n";
//            } else {
//                std::cout << "Unknown Type" << std::endl;
//            }
//        }
//        std::cout << "Total: " << scene.getPrimitives().size() << " primitives\n" << std::endl;
//
//        // === LIGHTS ===
//        std::cout << "[LIGHTS]" << std::endl;
//        int lightIdx = 0;
//        for (const auto& light : scene.getLights()) {
//            if (auto amb = dynamic_cast<AmbientLight*>(light.get())) {
//                std::cout << "    Ambient: " << amb->intensity << std::endl;
//            } else if (auto dif = dynamic_cast<DiffuseLight*>(light.get())) {
//                std::cout << "    Diffuse: " << dif->diffusion << std::endl;
//            } else if (auto point = dynamic_cast<PointLight*>(light.get())) {
//                std::cout << "    Position : (" << point->position.x << ", " << point->position.y << ", " << point->position.z << ")\n";
//            } else if (auto dir = dynamic_cast<DirectionalLight*>(light.get())) {
//                std::cout << "    Direction: (" << dir->direction.x << ", " << dir->direction.y << ", " << dir->direction.z << ")\n";
//            } else {
//                std::cout << "- Light #" << lightIdx++ << ": Unknown\n";
//            }
//        }
//
//    } catch (const std::exception& e) {
//        std::cerr << "Error while loading scene: " << e.what() << std::endl;
//        return 84;
//    }
//
//    return 0;
//}
//