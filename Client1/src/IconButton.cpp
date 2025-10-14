#include "Menu.hpp"

IconButton::IconButton() {
    isHovered = false;
    isToggled = false;
}

IconButton::IconButton(const std::string& iconType, float x, float y, float radius)
    : centerX(x), centerY(y), type(iconType) {
    circle.setPosition(x - radius, y - radius);
    circle.setRadius(radius);
    circle.setFillColor(sf::Color(0, 255, 255, 30));
    circle.setOutlineColor(sf::Color::Cyan);
    circle.setOutlineThickness(2.f);

    if (iconType == "mute") {
        createMuteIcon(x, y, radius * 0.6f);
    } else if (iconType == "stop") {
        createStopIcon(x, y, radius * 0.6f);
    }
}

void IconButton::createMuteIcon(float x, float y, float size) {
    sf::RectangleShape speaker1(sf::Vector2f(size * 0.4f, size * 0.3f));
    speaker1.setPosition(x - size * 0.5f, y - size * 0.15f);
    speaker1.setFillColor(sf::Color::Cyan);
    shapes.push_back(speaker1);

    sf::RectangleShape speaker2(sf::Vector2f(size * 0.5f, size * 0.7f));
    speaker2.setPosition(x - size * 0.15f, y - size * 0.35f);
    speaker2.setFillColor(sf::Color::Cyan);
    shapes.push_back(speaker2);

    sf::RectangleShape line(sf::Vector2f(size * 1.2f, 3.f));
    line.setPosition(x - size * 0.6f, y);
    line.setFillColor(sf::Color::Red);
    line.setRotation(45.f);
    line.setOrigin(0, 1.5f);
    shapes.push_back(line);
}

void IconButton::createStopIcon(float x, float y, float size) {
    sf::RectangleShape stopSquare(sf::Vector2f(size, size));
    stopSquare.setPosition(x - size * 0.5f, y - size * 0.5f);
    stopSquare.setFillColor(sf::Color::Cyan);
    shapes.push_back(stopSquare);
}

void IconButton::update(const sf::Vector2f& mousePos) {
    sf::FloatRect bounds = circle.getGlobalBounds();
    isHovered = bounds.contains(mousePos.x, mousePos.y);

    sf::Color shapeColor;
    if (isHovered) {
        circle.setFillColor(sf::Color(255, 107, 53, 60));
        circle.setOutlineColor(sf::Color(255, 107, 53));
        shapeColor = sf::Color(255, 107, 53);
    } else {
        if (isToggled) {
            circle.setFillColor(sf::Color(255, 0, 0, 40));
            circle.setOutlineColor(sf::Color::Red);
            shapeColor = sf::Color::Red;
        } else {
            circle.setFillColor(sf::Color(0, 255, 255, 30));
            circle.setOutlineColor(sf::Color::Cyan);
            shapeColor = sf::Color::Cyan;
        }
    }

    for (auto& shape : shapes) {
        if (type == "mute" && &shape == &shapes.back()) {
            shape.setFillColor(isToggled ? sf::Color::Red : sf::Color(255, 0, 0, 100));
        } else {
            shape.setFillColor(shapeColor);
        }
    }
}

void IconButton::draw(sf::RenderTarget& target) const {
    target.draw(circle);
    for (const auto& shape : shapes)
        target.draw(shape);
}

void IconButton::onClick() {
    if (isHovered)
        isToggled = !isToggled;
}
