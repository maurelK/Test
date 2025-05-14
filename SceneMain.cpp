#include "Scene.hpp"
#include <iostream>

//int main(int ac, char **av)
//{
//    if (ac != 2) {
//        std::cerr << "Usage: " << av[0] << " <scene_config_file>" << std::endl;
//        return 84;
//    }
//    Scene scene;
//    try {
//        scene.load_scene(av[1]);
//    } catch (const std::exception& ex) {
//        std::cerr << "Error loading scene: " << ex.what() << std::endl;
//        return 84;
//    }
//    std::cout << "Scene loaded successfully." << std::endl;
//    return 0;
//}

#include "Scene.hpp"
#include "Camera.hpp"
#include "Vec3.hpp"
#include "IPrimitive.hpp"
#include "ILight.hpp"
#include <iostream>
#include <memory>

int main(int argc, char** argv) {
    if (argc != 2) {
        std::cerr << "Usage: " << argv[0] << " <scene.cfg>" << std::endl;
        return 84;
    }

    Scene scene;

    try {
        scene.load_scene(argv[1]);
        std::cout << "Scene loaded successfully ✅\n" << std::endl;

        // === Camera ===
        const Camera& cam = scene.getCamera();
        std::cout << "[CAMERA]" << std::endl;
        std::cout << "Resolution: " << cam.width << " x " << cam.height << std::endl;
        std::cout << "Position  : (" << cam.position.x << ", " << cam.position.y << ", " << cam.position.z << ")\n";
        std::cout << "Rotation  : (" << cam.rotation.x << ", " << cam.rotation.y << ", " << cam.rotation.z << ")\n";
        std::cout << "FOV       : " << cam.fieldOfView << "\n" << std::endl;

        // === Primitives ===
        std::cout << "[PRIMITIVES]" << std::endl;
        int idx = 0;
        for (const auto& prim : scene.getPrimitives()) {
            std::cout << "- Primitive #" << idx++ << " loaded (type unknown statically)\n";
            // Bonus: tu peux plus tard faire un cast dynamique ici
        }
        std::cout << "Total: " << scene.getPrimitives().size() << " primitives\n" << std::endl;

        // === Lights ===
        std::cout << "[LIGHTS]" << std::endl;
        idx = 0;
        for (const auto& light : scene.getLights()) {
            if (dynamic_cast<AmbientLight*>(light.get()))
                std::cout << "- Light #" << idx++ << ": Ambient\n";
            else if (dynamic_cast<PointLight*>(light.get()))
                std::cout << "- Light #" << idx++ << ": Point\n";
            else if (dynamic_cast<DirectionalLight*>(light.get()))
                std::cout << "- Light #" << idx++ << ": Directional\n";
            else
                std::cout << "- Light #" << idx++ << ": Unknown\n";
        }

    } catch (const std::exception& e) {
        std::cerr << "⚠️  Error while loading scene: " << e.what() << std::endl;
        return 84;
    }

    return 0;
}
