#include "Lobby.hpp"
#include <iostream>

// ===== TextField =====
TextField::TextField(const sf::Font& font, const std::string& labelText,
                     float x, float y, float width, float height)
    : text(""), isActive(false), isHovered(false), cursorTime(0.f) {

    label.setFont(font);
    label.setString(labelText);
    label.setCharacterSize(18);
    label.setFillColor(sf::Color::Cyan);
    label.setPosition(x, y - 25);

    box.setPosition(x, y);
    box.setSize({width, height});
    box.setFillColor(sf::Color(20, 20, 40, 200));
    box.setOutlineColor(sf::Color::Cyan);
    box.setOutlineThickness(2.f);

    content.setFont(font);
    content.setCharacterSize(20);
    content.setFillColor(sf::Color::White);
    content.setPosition(x + 10, y + height / 2);
}

void TextField::update(const sf::Vector2f& mousePos, float deltaTime) {
    isHovered = box.getGlobalBounds().contains(mousePos);

    if (isActive) {
        box.setOutlineColor(sf::Color(255, 107, 53));
        box.setFillColor(sf::Color(40, 40, 60, 220));
        cursorTime += deltaTime;
        if (cursorTime > 1.0f) cursorTime = 0.f;

        std::string displayText = text;
        if (cursorTime < 0.5f) displayText += "|";
        content.setString(displayText);
    } else {
        box.setOutlineColor(isHovered ? sf::Color(255, 107, 53) : sf::Color::Cyan);
        box.setFillColor(isHovered ? sf::Color(30, 30, 50, 200) : sf::Color(20, 20, 40, 200));
        content.setString(text);
    }
}

void TextField::draw(sf::RenderTarget& target) const {
    target.draw(label);
    target.draw(box);
    target.draw(content);
}

void TextField::handleInput(sf::Uint32 unicode) {
    if (!isActive || text.length() >= 30) return;
    if (unicode >= 32 && unicode < 127)
        text += static_cast<char>(unicode);
}

void TextField::handleBackspace() {
    if (isActive && !text.empty()) text.pop_back();
}

void TextField::setActive(bool active) {
    isActive = active;
    cursorTime = 0.f;
}

bool TextField::contains(const sf::Vector2f& point) const {
    return box.getGlobalBounds().contains(point);
}

// ===== LobbyButton =====
LobbyButton::LobbyButton(const sf::Font& font, const std::string& label,
                         float x, float y, float width, float height)
    : isHovered(false) {

    rect.setPosition(x, y);
    rect.setSize({width, height});
    rect.setFillColor(sf::Color(0, 255, 255, 50));
    rect.setOutlineColor(sf::Color::Cyan);
    rect.setOutlineThickness(3.f);

    text.setFont(font);
    text.setString(label);
    text.setCharacterSize(18);
    text.setFillColor(sf::Color::Cyan);
    text.setStyle(sf::Text::Bold);

    float rightMargin = 140.f;
    sf::FloatRect textBounds = text.getLocalBounds();
    text.setOrigin(textBounds.left + textBounds.width, textBounds.top + textBounds.height / 2.f);
    text.setPosition(x + width - rightMargin, y + height / 2.f);
}

void LobbyButton::update(const sf::Vector2f& mousePos) {
    isHovered = rect.getGlobalBounds().contains(mousePos);
    if (isHovered) {
        rect.setFillColor(sf::Color(255, 107, 53, 80));
        rect.setOutlineColor(sf::Color(255, 107, 53));
        text.setFillColor(sf::Color(255, 107, 53));
    } else {
        rect.setFillColor(sf::Color(0, 255, 255, 50));
        rect.setOutlineColor(sf::Color::Cyan);
        text.setFillColor(sf::Color::Cyan);
    }
}

void LobbyButton::draw(sf::RenderTarget& target) const {
    target.draw(rect);
    target.draw(text);
}

bool LobbyButton::isClicked(const sf::Vector2f& mousePos) const {
    return rect.getGlobalBounds().contains(mousePos);
}

// ===== Lobby =====
Lobby::Lobby(const sf::RenderWindow& window)
    : playerNameField(font, "NOM DU JOUEUR:", 250, 200, 300, 50),
      serverIPField(font, "ADRESSE IP:", 250, 290, 300, 50),
      serverPortField(font, "PORT:", 250, 380, 300, 50),
      connectButton(font, "CONNEXION", 250, 480, 150, 50),
      backButton(font, "RETOUR", 410, 480, 140, 50),
      state(LobbyState::Idle),
      activeField(nullptr) {

    if (!font.loadFromFile("assets/logo_font.ttf"))
        std::cerr << "Erreur: Impossible de charger la police\n";

    if (backgroundTexture.loadFromFile("assets/covermeny.png")) {
        backgroundSprite.setTexture(backgroundTexture);
        backgroundSprite.setScale(
            static_cast<float>(window.getSize().x) / backgroundTexture.getSize().x,
            static_cast<float>(window.getSize().y) / backgroundTexture.getSize().y
        );
    }

    titleText.setFont(font);
    titleText.setString("LOBBY MULTIJOUEUR");
    titleText.setCharacterSize(40);
    titleText.setFillColor(sf::Color::Cyan);
    titleText.setStyle(sf::Text::Bold);
    sf::FloatRect titleBounds = titleText.getLocalBounds();
    titleText.setOrigin(titleBounds.width / 2.f, 0);
    titleText.setPosition(window.getSize().x / 2.f, 80);

    statusText.setFont(font);
    statusText.setCharacterSize(18);
    statusText.setFillColor(sf::Color::Yellow);
    statusText.setPosition(window.getSize().x / 2.f, 550);

    playerNameField.text = "Player1";
    serverIPField.text = "127.0.0.1";
    serverPortField.text = "9091";
}

void Lobby::update(const sf::Vector2f& mousePos, float deltaTime) {
    playerNameField.update(mousePos, deltaTime);
    serverIPField.update(mousePos, deltaTime);
    serverPortField.update(mousePos, deltaTime);
    connectButton.update(mousePos);
    backButton.update(mousePos);
}

void Lobby::draw(sf::RenderTarget& target) const {
    target.draw(backgroundSprite);

    sf::RectangleShape overlay(sf::Vector2f(target.getSize().x, target.getSize().y));
    overlay.setFillColor(sf::Color(0, 0, 0, 150));
    target.draw(overlay);

    target.draw(titleText);
    playerNameField.draw(target);
    serverIPField.draw(target);
    serverPortField.draw(target);
    connectButton.draw(target);
    backButton.draw(target);

    if (!statusText.getString().isEmpty())
        target.draw(statusText);
}

void Lobby::handleClick(const sf::Vector2f& mousePos) {
    playerNameField.setActive(false);
    serverIPField.setActive(false);
    serverPortField.setActive(false);
    activeField = nullptr;

    if (playerNameField.contains(mousePos)) {
        playerNameField.setActive(true);
        activeField = &playerNameField;
    } else if (serverIPField.contains(mousePos)) {
        serverIPField.setActive(true);
        activeField = &serverIPField;
    } else if (serverPortField.contains(mousePos)) {
        serverPortField.setActive(true);
        activeField = &serverPortField;
    }

    if (connectButton.isClicked(mousePos)) {
        std::cout << "[Lobby] Tentative de connexion..." << std::endl;
        setStatus("Connexion au serveur...", false);
        state = LobbyState::Connecting;
    } 
    else if (backButton.isClicked(mousePos)) {
        std::cout << "[Lobby] Retour à la sélection de mode" << std::endl;
        state = LobbyState::Back;
    }
}

void Lobby::handleTextInput(sf::Uint32 unicode) {
    if (activeField)
        activeField->handleInput(unicode);
}

void Lobby::handleBackspace() {
    if (activeField)
        activeField->handleBackspace();
}

void Lobby::setStatus(const std::string& status, bool isError) {
    statusText.setString(status);
    statusText.setFillColor(isError ? sf::Color::Red : sf::Color::Yellow);
    sf::FloatRect bounds = statusText.getLocalBounds();
    statusText.setOrigin(bounds.width / 2.f, 0);
}
