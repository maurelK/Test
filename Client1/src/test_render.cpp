#include "Orchestror.hpp"
#include "game/Gameplaycomponents.hpp"
#include "SFML/Graphics.hpp"
#include <iostream>
#include "game/RenderSystem.hpp"

int main()
{
    sf::RenderWindow window(sf::VideoMode(800, 600), "Test ECS- RenderSystem");
    window.setFramerateLimit(60);

    Orchestror ecs;
    ecs.init();
    ecs.registerComponent<Velocity>();
    ecs.registerComponent<Player>();
    ecs.registerComponent<Position>();
    ecs.registerComponent<Sprite>();
    ecs.registerComponent<PowerUp>();
    ecs.registerComponent<Projectile>();
    ecs.registerComponent<Health>();

    auto renderSystem = ecs.registerSystem<RenderSystem>(ecs, window);

    Signature sig;
    sig.set(ecs.getComponentType<Position>());
    sig.set(ecs.getComponentType<Sprite>());
    ecs.setSystemSignature<RenderSystem>(sig);

    Entity player = ecs.createEntity();

    ecs.addComponent(player, Position{200.f, 200.f});

    sf::Texture* tex = new sf::Texture();
    if (!tex->loadFromFile("assets/player.png")) {
        std::cerr << " Erreur : impossible de charger assets/player.png" << std::endl;
        return 1;
    } else {
        std::cout << " [DBG] Texture chargée avec succès !" << std::endl;
    }

    sf::Sprite spr(*tex);
    spr.setScale(3.f, 3.f);
    ecs.addComponent(player, Sprite{spr});

    ecs.update(0.f);

    while (window.isOpen()) {
        sf::Event event;
        while (window.pollEvent(event)) {
            if (event.type == sf::Event::Closed)
                window.close();
        }

        window.clear(sf::Color::Black);

        renderSystem->update(1.f / 60.f);

        window.display();
    }

    return 0;
}
