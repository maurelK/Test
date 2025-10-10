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
    text.setPosition(x + 20.f, y + height/2 - 10.f);
}

void Button::update(const sf::Vector2f& mousePos, bool isSelected) {
    sf::FloatRect bounds = rect.getGlobalBounds();
    isHovered = bounds.contains(mousePos.x, mousePos.y);

    // Priorité: sélection clavier > survol souris
    bool highlighted = isSelected || isHovered;

    if (highlighted) {
        rect.setFillColor(sf::Color(255, 107, 53, 50));
        rect.setOutlineColor(sf::Color(255, 107, 53));
        text.setFillColor(sf::Color(255, 107, 53));
        rect.setPosition(originalX + 10.f, originalY);
        text.setPosition(originalX + 30.f, originalY + rect.getSize().y/2 - 10.f);
    } else {
        rect.setFillColor(sf::Color(0, 255, 255, 25));
        rect.setOutlineColor(sf::Color::Cyan);
        text.setFillColor(sf::Color::Cyan);
        rect.setPosition(originalX, originalY);
        text.setPosition(originalX + 20.f, originalY + rect.getSize().y/2 - 10.f);
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
            if (isToggled) {
                shape.setFillColor(sf::Color::Red);
            } else {
                shape.setFillColor(sf::Color(255, 0, 0, 100));
            }
        } else {
            shape.setFillColor(shapeColor);
        }
    }
}

void IconButton::draw(sf::RenderTarget& target) const {
    target.draw(circle);
    for (const auto& shape : shapes) {
        target.draw(shape);
    }
}

void IconButton::onClick() {
    if (isHovered) {
        isToggled = !isToggled;
    }
}

Menu::Menu(const sf::RenderWindow& window) {
    if (!font.loadFromFile("assets/logo_font.ttf")) {
        if (!font.loadFromFile("logo_font.ttf")) {
            return;
        }
    }

    float w = static_cast<float>(window.getSize().x);
    float h = static_cast<float>(window.getSize().y);
    
    float btnW = 300.f;
    float btnH = 50.f;
    float btnSpacing = 70.f;
    float startY = h * 0.3f;

    std::vector<std::pair<std::string, std::string>> data = {
        {"NOUVELLE PARTIE", "NewGame"},
        {"CONTINUER", "Continue"},
        {"OPTIONS", "Options"},
        {"MEILLEURS SCORES", "HighScore"},
        {"CREDITS", "Credits"},
        {"QUITTER", "Quit"}
    };

    float btnY = startY;
    for (const auto& d : data) {
        float btnX = (w - btnW) / 2;
        buttons.emplace_back(font, d.first, d.second, btnX, btnY, btnW, btnH);
        btnY += btnSpacing;
    }

    float iconRadius = 25.f;
    float iconY = 40.f;
    
    stopButton = IconButton("stop", w - 120.f, iconY, iconRadius);
    muteButton = IconButton("mute", w - 60.f, iconY, iconRadius);
}

void Menu::update(const sf::Vector2f& mousePos) {
    for (size_t i = 0; i < buttons.size(); ++i) {
        buttons[i].update(mousePos, i == static_cast<size_t>(selectedButtonIndex));
    }
    stopButton.update(mousePos);
    muteButton.update(mousePos);
}

void Menu::draw(sf::RenderTarget& target) const {
    for (const auto& btn : buttons) {
        btn.draw(target);
    }
    
    stopButton.draw(target);
    muteButton.draw(target);
}

MenuState Menu::handleClick(const sf::Vector2f& mousePos) {
    if (stopButton.circle.getGlobalBounds().contains(mousePos)) {
        stopButton.onClick();
    }
    if (muteButton.circle.getGlobalBounds().contains(mousePos)) {
        muteButton.onClick();
    }

    for (auto& btn : buttons) {
        if (btn.rect.getGlobalBounds().contains(mousePos) && btn.onClick()) {
            if (btn.action == "NewGame") return state = MenuState::NewGame;
            if (btn.action == "Continue") return state = MenuState::Continue;
            if (btn.action == "Options") return state = MenuState::Options;
            if (btn.action == "HighScore") return state = MenuState::HighScore;
            if (btn.action == "Credits") return state = MenuState::Credits;
            if (btn.action == "Quit") return state = MenuState::Quit;
        }
    }
    return MenuState::Main;
}

// Navigation clavier - Monter dans le menu
void Menu::navigateUp() {
    if (selectedButtonIndex > 0) {
        selectedButtonIndex--;
    } else {
        // Boucle vers le bas
        selectedButtonIndex = static_cast<int>(buttons.size()) - 1;
    }
}

// Navigation clavier - Descendre dans le menu
void Menu::navigateDown() {
    if (selectedButtonIndex < static_cast<int>(buttons.size()) - 1) {
        selectedButtonIndex++;
    } else {
        // Boucle vers le haut
        selectedButtonIndex = 0;
    }
}

// Sélectionner le bouton actuel avec le clavier
MenuState Menu::selectCurrentButton() {
    if (selectedButtonIndex >= 0 && selectedButtonIndex < static_cast<int>(buttons.size())) {
        const auto& btn = buttons[selectedButtonIndex];
        if (btn.action == "NewGame") return state = MenuState::NewGame;
        if (btn.action == "Continue") return state = MenuState::Continue;
        if (btn.action == "Options") return state = MenuState::Options;
        if (btn.action == "HighScore") return state = MenuState::HighScore;
        if (btn.action == "Credits") return state = MenuState::Credits;
        if (btn.action == "Quit") return state = MenuState::Quit;
    }
    return MenuState::Main;
}
