#include "Logo.hpp"
#include <string>
#include <cmath>

// Constructeur : Charge police, set texte "R-TYPE", centre en haut (sans outline)
Logo::Logo(const sf::RenderWindow& window) {
    if (!font.loadFromFile("assets/logo_font.ttf")) {
    }

    text.setFont(font);
    text.setString("R-TYPE");  // Texte du logo
    text.setCharacterSize(72);  // Taille grande et visible
    text.setFillColor(sf::Color(255, 255, 0));  // Jaune vif (contraste fort)
    text.setStyle(sf::Text::Bold);  // Gras pour impact

    sf::FloatRect bounds = text.getLocalBounds();
    text.setOrigin(bounds.width / 2, bounds.height / 2);
    text.setPosition(static_cast<float>(window.getSize().x) / 2, 40.f);
}

void Logo::update(float deltaTime) {
    pulsationTime += deltaTime;
    float scale = 1.f + 0.1f * sin(pulsationTime * pulsationSpeed * 2 * 3.14159f);  // +10% max (cycle doux)
    text.setScale(scale, scale);
}

// Dessin : Ombre bleue (décalée + foncée) + texte principal jaune (<10 lignes)
void Logo::draw(sf::RenderTarget& target) const {
    sf::Text shadow = text;
    shadow.setPosition(text.getPosition() + sf::Vector2f(5.f, 5.f));
    shadow.setFillColor(sf::Color(0, 20, 50, 220));
    target.draw(shadow);
    target.draw(text);
}