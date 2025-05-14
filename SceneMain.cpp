#include "Scene.hpp"
#include <iostream>

int main(int ac, char **av)
{
    if (ac != 2) {
        std::cerr << "Usage: " << av[0] << " <scene_config_file>" << std::endl;
        return 84;
    }
    Scene scene;
    try {
        scene.load_scene(av[1]);
    } catch (const std::exception& ex) {
        std::cerr << "Error loading scene: " << ex.what() << std::endl;
        return 84;
    }
    std::cout << "Scene loaded successfully." << std::endl;
    return 0;
}
