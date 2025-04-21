#include "../include/arcade_sfml.hpp"
#include <iostream>
#include <stdexcept>

ArcadeSFML::ArcadeSFML() : window(nullptr) {}

ArcadeSFML::~ArcadeSFML() 
{
    close();
}

bool ArcadeSFML::init()
{
    window = new sf::RenderWindow(sf::VideoMode(1600, 800), "Arcade Game");

    if (!font.loadFromFile("assets/font.ttf")) {
        std::cerr << "Warning: Could not load font, using default SFML font" << std::endl;
        return false;
    }
    return true;
}

void ArcadeSFML::close() 
{
    if (window) {
        window->close();
        delete window;
        window = nullptr;
    }
}

sf::Color ArcadeSFML::getColorFromCode(int colorCode) 
{
    switch (colorCode) {
        case 1: return sf::Color::White;
        case 2: return sf::Color::Red;
        case 3: return sf::Color::Green;
        case 4: return sf::Color::Blue;
        case 5: return sf::Color::Yellow;
        default: return sf::Color::White;
    }
}

void ArcadeSFML::renderText(const std::string &text, int x, int y, sf::Color color) 
{
    sf::Text sfmlText;
    sfmlText.setFont(font);
    sfmlText.setString(text);
    sfmlText.setCharacterSize(24);
    sfmlText.setFillColor(color);
    sfmlText.setPosition(x, y);
    window->draw(sfmlText);
}

void ArcadeSFML::render(const RenderData &data) 
{
    if (!window) return;

    window->clear(sf::Color::Black);

    for (const auto &entity : data.entities) {
        sf::RectangleShape rect(sf::Vector2f(10.f, 10.f));
        rect.setPosition(entity.x * 10.f, entity.y * 10.f);
        rect.setFillColor(getColorFromCode(entity.color));
        window->draw(rect);
    }
    for (const auto &text : data.texts) {
        renderText(text.content, text.x * 10, text.y * 25, getColorFromCode(text.color));
    }
    window->display();
    sf::sleep(sf::milliseconds(16));
}

int ArcadeSFML::getInput() 
{
    sf::Event event;
    while (window->pollEvent(event)) {
        if (event.type == sf::Event::Closed) {
            close();
            return 27;
        }
        if (event.type == sf::Event::KeyPressed) {
            switch (event.key.code) {
                case sf::Keyboard::Up: return 0;
                case sf::Keyboard::Down: return 1;
                case sf::Keyboard::Left: return 2;
                case sf::Keyboard::Right: return 3;
                case sf::Keyboard::Return: return 10;
                case sf::Keyboard::N: return 'n';
                case sf::Keyboard::Escape: return 27;
                default: return event.key.code; 
            }
        }
    }
    return -1;
}

std::string ArcadeSFML::getPlayerName() {
    return "Player";
}

extern "C" {
    IGraphical *createGraphical() {
        return new ArcadeSFML();
    }

    void deleteGraphical(IGraphical *graphical) {
        delete static_cast<ArcadeSFML *>(graphical);
    }
}
