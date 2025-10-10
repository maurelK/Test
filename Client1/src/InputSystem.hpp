#pragma once

#include "Orchestror.hpp"
#include "../rtype_engine/Components.hpp"
#include  <SFML/Window/Keyboard.hpp>
#include "System.hpp"

class InputSystem : public System {
    public:
        InputSystem(Orchestror& o) : orchestror(o) {}

        void update(float dt) override {
            for(auto entity : entities) {
                if(orchestror.hasComponent<Velocity>(entity)) {
                    auto& vel = orchestror.getComponent<Velocity>(entity);
                    vel.dx = vel.dy = 0.f;

                    if (sf::Keyboard::isKeyPressed(sf::Keyboard::Left))
                        vel.dx = -100.f;
                    if (sf::Keyboard::isKeyPressed(sf::Keyboard::Right))
                        vel.dx = 100.f;
                    if (sf::Keyboard::isKeyPressed(sf::Keyboard::Up))
                        vel.dy = -100.f;
                    if (sf::Keyboard::isKeyPressed(sf::Keyboard::Down))
                        vel.dy = 100.f;
                }
            }
        }

    private:
        Orchestror& orchestror;
};