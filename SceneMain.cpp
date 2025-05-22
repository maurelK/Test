#include "Scene.hpp"
#include "RayGenerator.hpp"
#include <SFML/Graphics.hpp>
#include <iostream>
#include <fstream>
#include <sstream>
#include <cmath>

sf::Image loadPPMToSFMLImage(const std::string& filename) {
    std::ifstream file(filename);
    if (!file)
        throw std::runtime_error(" Cannot open PPM file");

    std::string line;
    std::getline(file, line);
    if (line != "P3")
        throw std::runtime_error(" Only ASCII PPM (P3) supported");
    do {
        std::getline(file, line);
    } while (line[0] == '#');

    int width, height, maxColor;
    std::istringstream dimStream(line);
    dimStream >> width >> height;
    file >> maxColor;

    sf::Image image;
    image.create(width, height);

    int r, g, b;
    for (int y = 0; y < height; ++y)
        for (int x = 0; x < width; ++x) {
            file >> r >> g >> b;
            image.setPixel(x, y, sf::Color(r, g, b));
        }

    return image;
}

int main(int argc, char** argv) {
    if (argc != 2) {
        std::cerr << "Usage: " << argv[0] << " <scene_config_file>" << std::endl;
        return 84;
    }

    Scene scene;
    std::string filename = "render.ppm";

    try {
        scene.load_scene(argv[1]);

        const Camera& cam = scene.getCamera();
        RayGenerator rayGen(cam, cam.width, cam.height);
        scene.render(cam.width, cam.height, rayGen, filename);
        sf::Image image = loadPPMToSFMLImage(filename);
        sf::Texture texture;
        texture.loadFromImage(image);
        sf::Sprite sprite(texture);

        sf::RenderWindow window(sf::VideoMode(cam.width, cam.height), "Raytracer Output");
        window.setFramerateLimit(60);

        
        

        while (window.isOpen()) {
            sf::Event event;
            while (window.pollEvent(event)) {
                if (event.type == sf::Event::Closed)
                    window.close();
            }
            window.clear(sf::Color::Black);
            window.draw(sprite);
            window.display();
        }

    } catch (const std::exception& e) {
        return 84;
    }

    return 0;
}