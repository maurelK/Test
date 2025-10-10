#include "Orchestror.hpp"
#include "../rtype_engine/Components.hpp"
#include "MovementSystem.hpp"
#include <SFML/Graphics.hpp>
#include <iostream>


int main() {
    Orchestror ecs;
    ecs.init();

    ecs.registerComponent<Position>();
    ecs.registerComponent<Velocity>();

    auto movSys = ecs.registerSystem<MovementSystem>(ecs);

    Signature sig;
    sig.set(ecs.getComponentType<Position>());
    sig.set(ecs.getComponentType<Velocity>());
    ecs.setSystemSignature<MovementSystem>(sig);

    Entity e = ecs.createEntity();

    ecs.addComponent(e, Position{100.f, 300.f});
    ecs.addComponent(e, Velocity{50.f, 0.f});

    sf::RenderWindow window(sf::VideoMode(800,600), "Test MoveSystm");
    window.setFramerateLimit(60);

    sf::CircleShape shape(20.f);
    shape.setFillColor(sf::Color::Cyan);

    while(window.isOpen()) {
        sf::Event event;
        while (window.pollEvent(event))
        if(event.type == sf::Event::Closed)
        window.close();

        ecs.update(1.f / 60.f);

        auto& pos = ecs .getComponent<Position>(e);
        shape.setPosition(pos.x, pos.y);

        window.clear();
        window.draw(shape);
        window.display();
    }
}
