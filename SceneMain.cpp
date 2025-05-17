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
#include "Sphere.hpp"
#include "Plane.hpp"
#include <iostream>
#include <memory>

#include "Scene.hpp"
#include "Camera.hpp"
#include "RayGenerator.hpp"
#include <iostream>

int main(int argc, char** argv) {
    if (argc != 2) {
        std::cerr << "Usage: " << argv[0] << " <scene_config_file>" << std::endl;
        return 84;
    }

    Scene scene;

    try {
        scene.load_scene(argv[1]);
        std::cout << " Scene loaded successfully.\n";

        const Camera& cam = scene.getCamera();
        RayGenerator rayGen(cam, cam.width, cam.height);

        std::string filename = "render.ppm";
        std::cout << " Rendering to: " << filename << std::endl;

        scene.render(cam.width, cam.height, rayGen, filename);
        std::cout << " Render complete.\n";

    } catch (const std::exception& e) {
        std::cerr << " Error: " << e.what() << std::endl;
        return 84;
    }

    return 0;
}
