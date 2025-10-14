#include "ModeSelection.hpp"
#include <iostream>
#include <algorithm>

ModeSelection::ModeSelection(sf::RenderWindow& win)
    : window(win), selectedIndex(0), pendingSelection(GameMode::None) 
{
    if (!font.loadFromFile("assets/logo_font.ttf")) {
        std::cerr << "[ModeSelection] Erreur chargement police\n";
    }

    initBackground();
    initTitle();
    initButtons();
    initInstructions();
}

ModeSelection::~ModeSelection() {}

void ModeSelection::initBackground() {
    if (!backgroundTexture.loadFromFile("assets/covermeny.png")) {
        std::cerr << "[ModeSelection] Erreur chargement covermeny.png\n";
        return;
    }

    backgroundSprite.setTexture(backgroundTexture);

    sf::Vector2u texSize = backgroundTexture.getSize();
    sf::Vector2u winSize = window.getSize();

    // ✅ Remplir tout l’écran sans déformation visible
    float scaleX = static_cast<float>(winSize.x) / texSize.x;
    float scaleY = static_cast<float>(winSize.y) / texSize.y;
    float scale = std::max(scaleX, scaleY);

    backgroundSprite.setScale(scale, scale);

    // ✅ Centrer l’image
    float offsetX = (winSize.x - texSize.x * scale) / 2.f;
    float offsetY = (winSize.y - texSize.y * scale) / 2.f;
    backgroundSprite.setPosition(offsetX, offsetY);
}

void ModeSelection::initTitle() {
    titleText.setFont(font);
    titleText.setString("SELECTION DU MODE");
    titleText.setCharacterSize(48);
    titleText.setFillColor(sf::Color::Cyan);
    titleText.setStyle(sf::Text::Bold);

    sf::FloatRect bounds = titleText.getLocalBounds();
    titleText.setOrigin(bounds.width / 2, bounds.height / 2);
    titleText.setPosition(window.getSize().x / 2.f, 100);
}

void ModeSelection::initButtons() {
    std::vector<std::pair<std::string, GameMode>> buttonData = {
        {"MODE SOLO", GameMode::Solo},
        {"MODE MULTIJOUEUR", GameMode::Multiplayer},
        {"RETOUR AU MENU", GameMode::Back}
    };

    float startY = 250.f;
    float spacing = 100.f;
    float centerX = window.getSize().x / 2.f;

    for (size_t i = 0; i < buttonData.size(); i++) {
        Button btn;
        btn.shape.setSize(sf::Vector2f(400, 60));
        btn.shape.setPosition(centerX - 200.f, startY + i * spacing);
        btn.shape.setFillColor(sf::Color(255, 255, 255, 30));
        btn.shape.setOutlineThickness(2);
        btn.shape.setOutlineColor(sf::Color::Cyan);

        btn.text.setFont(font);
        btn.text.setString(buttonData[i].first);
        btn.text.setCharacterSize(28);
        btn.text.setFillColor(sf::Color::White);

        sf::FloatRect textBounds = btn.text.getLocalBounds();
        btn.text.setOrigin(textBounds.width / 2, textBounds.height / 2);
        btn.text.setPosition(centerX, startY + i * spacing + 30);

        btn.action = buttonData[i].second;
        btn.isHovered = false;

        buttons.push_back(btn);
    }

    // Met le premier bouton en surbrillance
    buttons[0].shape.setFillColor(sf::Color(0, 255, 255, 50));
    buttons[0].shape.setOutlineColor(sf::Color::White);
}

void ModeSelection::initInstructions() {
    instructionText.setFont(font);
    instructionText.setString("↑/↓ pour naviguer | ENTREE valider | ECHAP retour");
    instructionText.setCharacterSize(16);
    instructionText.setFillColor(sf::Color(255, 255, 255, 150));

    sf::FloatRect bounds = instructionText.getLocalBounds();
    instructionText.setOrigin(bounds.width / 2, 0);
    instructionText.setPosition(window.getSize().x / 2.f, window.getSize().y - 60);
}

void ModeSelection::navigateUp() {
    buttons[selectedIndex].shape.setFillColor(sf::Color(255, 255, 255, 30));
    buttons[selectedIndex].shape.setOutlineColor(sf::Color::Cyan);

    selectedIndex = (selectedIndex - 1 + buttons.size()) % buttons.size();

    buttons[selectedIndex].shape.setFillColor(sf::Color(0, 255, 255, 50));
    buttons[selectedIndex].shape.setOutlineColor(sf::Color::White);
}

void ModeSelection::navigateDown() {
    buttons[selectedIndex].shape.setFillColor(sf::Color(255, 255, 255, 30));
    buttons[selectedIndex].shape.setOutlineColor(sf::Color::Cyan);

    selectedIndex = (selectedIndex + 1) % buttons.size();

    buttons[selectedIndex].shape.setFillColor(sf::Color(0, 255, 255, 50));
    buttons[selectedIndex].shape.setOutlineColor(sf::Color::White);
}

GameMode ModeSelection::selectCurrent() {
    return buttons[selectedIndex].action;
}

void ModeSelection::updateButtonStates(sf::Vector2f mousePos) {
    for (size_t i = 0; i < buttons.size(); i++) {
        bool wasHovered = buttons[i].isHovered;
        buttons[i].isHovered = buttons[i].shape.getGlobalBounds().contains(mousePos);

        if (buttons[i].isHovered && !wasHovered) {
            buttons[selectedIndex].shape.setFillColor(sf::Color(255, 255, 255, 30));
            buttons[selectedIndex].shape.setOutlineColor(sf::Color::Cyan);

            selectedIndex = i;

            buttons[i].shape.setFillColor(sf::Color(0, 255, 255, 50));
            buttons[i].shape.setOutlineColor(sf::Color::White);
        }
    }
}

void ModeSelection::handleEvents() {
    sf::Event event;
    while (window.pollEvent(event)) {
        switch (event.type) {
            case sf::Event::Closed:
                window.close();
                pendingSelection = GameMode::Back;
                break;

            case sf::Event::KeyPressed:
                if (event.key.code == sf::Keyboard::Up)
                    navigateUp();
                else if (event.key.code == sf::Keyboard::Down)
                    navigateDown();
                else if (event.key.code == sf::Keyboard::Return || event.key.code == sf::Keyboard::Space)
                    pendingSelection = selectCurrent();
                else if (event.key.code == sf::Keyboard::Escape)
                    pendingSelection = GameMode::Back;
                break;

            case sf::Event::MouseButtonPressed:
                if (event.mouseButton.button == sf::Mouse::Left) {
                    sf::Vector2f mousePos = window.mapPixelToCoords(
                        { event.mouseButton.x, event.mouseButton.y });
                    for (const auto& btn : buttons) {
                        if (btn.shape.getGlobalBounds().contains(mousePos)) {
                            pendingSelection = btn.action;
                            break;
                        }
                    }
                }
                break;

            case sf::Event::Resized:
                // ✅ Ajuste la vue et le fond d’écran quand la fenêtre est redimensionnée
                window.setView(sf::View(sf::FloatRect(0, 0,
                    static_cast<float>(event.size.width),
                    static_cast<float>(event.size.height))));
                initBackground();
                break;

            default:
                break;
        }
    }
}

void ModeSelection::update(sf::Vector2f mousePos) {
    updateButtonStates(mousePos);
}

void ModeSelection::draw() {
    window.clear();
    window.draw(backgroundSprite);

    sf::RectangleShape overlay(sf::Vector2f(window.getSize().x, window.getSize().y));
    overlay.setFillColor(sf::Color(10, 10, 30, 100));
    window.draw(overlay);

    window.draw(titleText);

    for (const auto& btn : buttons) {
        window.draw(btn.shape);
        window.draw(btn.text);
    }

    window.draw(instructionText);
    window.display();
}

GameMode ModeSelection::run() {
    std::cout << "[ModeSelection] Ecran de selection lance\n";

    while (window.isOpen()) {
        handleEvents();

        if (pendingSelection != GameMode::None) {
            std::cout << "[ModeSelection] Choix: " << static_cast<int>(pendingSelection) << "\n";
            return pendingSelection;
        }

        sf::Vector2i mousePixelPos = sf::Mouse::getPosition(window);
        sf::Vector2f mousePos = window.mapPixelToCoords(mousePixelPos);
        update(mousePos);
        draw();
    }

    return GameMode::Back;
}
