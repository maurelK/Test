#ifndef LOGO_HPP
#define LOGO_HPP

#include <SFML/Graphics.hpp>

class Logo {
public:
    sf::Text text;
    sf::Font font;  // Police chargée
    float pulsationTime = 0.f;
    float pulsationSpeed = 2.f;  // Cycle en 2s

    Logo(const sf::RenderWindow& window);
    void update(float deltaTime);  // Animation pulsation
    void draw(sf::RenderTarget& target) const;  // Affichage avec ombre

};

#endif