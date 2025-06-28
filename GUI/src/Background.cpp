#include "Background.hpp"
#include <iostream>

Background::Background() : currentFrame(0) {}

void Background::initialize()
{
    frames.resize(15);
    for (size_t i = 1; i < frames.size(); ++i)
    {
        sf::Texture *texture = new sf::Texture();
        std::string filename = "asserts/background/ezgif-frame-0" +
                               (i < 10 ? "0" + std::to_string(i) : std::to_string(i)) +
                               ".png";
        if (!texture->loadFromFile(filename))
        {
            std::cerr << "Failed to load image: " << filename << std::endl;
        }
        frames[i].setTexture(*texture);
    }
}

void Background::update(float deltaTime)
{
    if (clock.getElapsedTime().asSeconds() > deltaTime)
    {
        currentFrame = (currentFrame + 1) % frames.size();
        clock.restart();
    }
}

void Background::draw(sf::RenderWindow &window)
{
    if (currentFrame < frames.size())
    {
        window.draw(frames[currentFrame]);
    }
}

Background::~Background()
{
    for (auto &frame : frames)
    {
        sf::Texture *texture = const_cast<sf::Texture *>(frame.getTexture());
        if (texture)
        {
            delete texture;
        }
    }
}
