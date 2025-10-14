#include "Menu.hpp"

Button::Button(const sf::Font& f, const std::string& label, const std::string& act, 
               float x, float y, float width, float height)
    : font(&f), action(act), originalX(x), originalY(y) {
    rect.setPosition(x, y);
    rect.setSize(sf::Vector2f(width, height));
    rect.setFillColor(sf::Color(0, 255, 255, 25));
    rect.setOutlineColor(sf::Color::Cyan);
    rect.setOutlineThickness(2.f);

    text.setFont(*font);
    text.setString(label);
    text.setCharacterSize(22);
    text.setFillColor(sf::Color::Cyan);
    text.setStyle(sf::Text::Bold);
    text.setPosition(x + 20.f, y + height / 2 - 10.f);
}

void Button::update(const sf::Vector2f& mousePos, bool isSelected) {
    sf::FloatRect bounds = rect.getGlobalBounds();
    isHovered = bounds.contains(mousePos.x, mousePos.y);

    bool highlighted = isSelected || isHovered;

    if (highlighted) {
        rect.setFillColor(sf::Color(255, 107, 53, 50));
        rect.setOutlineColor(sf::Color(255, 107, 53));
        text.setFillColor(sf::Color(255, 107, 53));
        rect.setPosition(originalX + 10.f, originalY);
        text.setPosition(originalX + 30.f, originalY + rect.getSize().y / 2 - 10.f);
    } else {
        rect.setFillColor(sf::Color(0, 255, 255, 25));
        rect.setOutlineColor(sf::Color::Cyan);
        text.setFillColor(sf::Color::Cyan);
        rect.setPosition(originalX, originalY);
        text.setPosition(originalX + 20.f, originalY + rect.getSize().y / 2 - 10.f);
    }

    if (isClicked) {
        isClicked = false;
    }
}

void Button::draw(sf::RenderTarget& target) const {
    target.draw(rect);
    target.draw(text);
}

bool Button::onClick() {
    if (isHovered) {
        isClicked = true;
        return true;
    }
    return false;
}
