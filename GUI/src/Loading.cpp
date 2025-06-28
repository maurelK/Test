#include "Loading.hpp"

Loading::Loading() : progress(0.0f) {}

void Loading::initialize()
{
    texture.loadFromFile("asserts/loading/loading.png");
    sprite.setTexture(texture);
    size = texture.getSize();
    sprite.setPosition(718.0f, 988.0f - 100.0f);
}

void Loading::update(float progress)
{
    this->progress = progress;
    int index = static_cast<int>(progress * 4);
    sf::IntRect rect(0, 0, size.x * (index + 1) / 4, size.y);
    sprite.setTextureRect(rect);
}

sf::Sprite &Loading::getSprite()
{
    return sprite;
}
