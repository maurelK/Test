#pragma once
#include "../rtype_engine/Components.hpp"
#include "System.hpp"
#include "Orchestror.hpp"
#include <SFML/Graphics.hpp>
#include <iostream>

class RenderSystem : public System {
public:
    RenderSystem(Orchestror& o, sf::RenderWindow &w)
        : orchestror(o), window(w) {}

    void update(float dt) override {
        (void)dt;

        for (auto entity : entities) {
            if (orchestror.hasComponent<Position>(entity) &&
                orchestror.hasComponent<Sprite>(entity)) {

                auto& pos = orchestror.getComponent<Position>(entity);
                auto& spr = orchestror.getComponent<Sprite>(entity);

                spr.sprite.setPosition(pos.x, pos.y);

                auto tex = spr.sprite.getTexture();
                if (tex) {
                    auto size = tex->getSize();
                    float maxW = window.getSize().x * 0.8f;
                    float maxH = window.getSize().y * 0.8f;
                    float scale = std::min(maxW / size.x, maxH / size.y);
                    spr.sprite.setScale(scale, scale);
                }

                window.draw(spr.sprite);

                std::cout << "[RenderSystem] Dessine entité "
                          << entity << " à (" << pos.x << "," << pos.y << ")\n";
            }
        }
    }

private:
    Orchestror &orchestror;
    sf::RenderWindow &window;
};
