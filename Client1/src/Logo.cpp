#include "Logo.hpp"
#include <string>
#include <cmath>


Logo::Logo(const sf::RenderWindow& window) {
    if (!font.loadFromFile("assets/logo_font.ttf")) {
    }

    text.setFont(font);
    text.setString("R-TYPE");
    text.setCharacterSize(72);
    text.setFillColor(sf::Color(255, 255, 0));
    text.setStyle(sf::Text::Bold);
    sf::FloatRect bounds = text.getLocalBounds();
    text.setOrigin(bounds.width / 2, bounds.height / 2);
    text.setPosition(static_cast<float>(window.getSize().x) / 2, 40.f);
}

void Logo::update(float deltaTime) {
    pulsationTime += deltaTime;
    float scale = 1.f + 0.1f * sin(pulsationTime * pulsationSpeed * 2 * 3.14159f);
    text.setScale(scale, scale);
}

void Logo::draw(sf::RenderTarget& target) const {
    sf::Text shadow = text;
    shadow.setPosition(text.getPosition() + sf::Vector2f(5.f, 5.f));
    shadow.setFillColor(sf::Color(0, 20, 50, 220));
    target.draw(shadow);
    target.draw(text);
}