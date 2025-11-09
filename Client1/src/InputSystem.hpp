#pragma once

#include "../rtype_engine/Orchestror.hpp"
#include "../rtype_engine/Components.hpp"
#include  <SFML/Window/Keyboard.hpp>
#include "System.hpp"

class InputSystem : public System {
    public:
        InputSystem(Orchestror& o, NetworkClient& net) 
        : orchestror(o), network(net) {}

        void update(float dt) override {
            for(auto entity : entities) {
                if(orchestror.hasComponent<Velocity>(entity)) {
                    auto& vel = orchestror.getComponent<Velocity>(entity);
                    vel.dx = vel.dy = 0.f;

                    if (sf::Keyboard::isKeyPressed(sf::Keyboard::Left))
                        vel.dx = -1.f;
                    if (sf::Keyboard::isKeyPressed(sf::Keyboard::Right))
                        vel.dx = 1.f;
                    if (sf::Keyboard::isKeyPressed(sf::Keyboard::Up))
                        vel.dy = -1.f;
                    if (sf::Keyboard::isKeyPressed(sf::Keyboard::Down))
                        vel.dy = 1.f;
                    
                    bool shoot = sf::Keyboard::isKeyPressed(sf::Keyboard::Space);

                InputPacket packet{};
                packet.header.type = PacketType::INPUT;
                packet.header.size = sizeof(InputPacket);
                packet.player_id = entity;
                packet.move_x = vel.dx;
                packet.move_y = vel.dy;
                packet.shoot = shoot;

                network.sendInput(packet);
                }
            }
        }

    private:
        Orchestror& orchestror;
        NetworkClient& network;
};