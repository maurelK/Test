#include "ModeSelection.hpp"
#include <iostream>

ModeSelection::ModeSelection(sf::RenderWindow& win) 
    : window(win), selectedIndex(0) {
    
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
    
    float scaleX = static_cast<float>(winSize.x) / texSize.x;
    float scaleY = static_cast<float>(winSize.y) / texSize.y;
    float scale = std::max(scaleX, scaleY);
    
    backgroundSprite.setScale(scale, scale);
    
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
    titleText.setPosition(400, 100);
}

void ModeSelection::initButtons() {
    std::vector<std::pair<std::string, GameMode>> buttonData = {
        {"MODE SOLO", GameMode::Solo},
        {"MODE MULTIJOUEUR", GameMode::Multiplayer},
        {"RETOUR AU MENU", GameMode::Back}
    };
    
    float startY = 250.f;
    float spacing = 100.f;
    
    for (size_t i = 0; i < buttonData.size(); i++) {
        Button btn;
        
        btn.shape.setSize(sf::Vector2f(400, 60));
        btn.shape.setPosition(200, startY + i * spacing);
        btn.shape.setFillColor(sf::Color(255, 255, 255, 30));
        btn.shape.setOutlineThickness(2);
        btn.shape.setOutlineColor(sf::Color::Cyan);
        
        btn.text.setFont(font);
        btn.text.setString(buttonData[i].first);
        btn.text.setCharacterSize(28);
        btn.text.setFillColor(sf::Color::White);
        
        sf::FloatRect textBounds = btn.text.getLocalBounds();
        btn.text.setOrigin(textBounds.width / 2, textBounds.height / 2);
        btn.text.setPosition(400, startY + i * spacing + 30);
        
        btn.action = buttonData[i].second;
        btn.isHovered = false;
        
        buttons.push_back(btn);
    }
    
    buttons[0].shape.setFillColor(sf::Color(0, 255, 255, 50));
    buttons[0].shape.setOutlineColor(sf::Color::White);
}

void ModeSelection::initInstructions() {
    instructionText.setFont(font);
    instructionText.setString("Fleches Haut/Bas | ENTREE Valider | ECHAP Retour");
    instructionText.setCharacterSize(16);
    instructionText.setFillColor(sf::Color(255, 255, 255, 150));
    
    sf::FloatRect bounds = instructionText.getLocalBounds();
    instructionText.setOrigin(bounds.width / 2, 0);
    instructionText.setPosition(400, 550);
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
        if (event.type == sf::Event::Closed) {
            window.close();
        }
        
        if (event.type == sf::Event::KeyPressed) {
            if (event.key.code == sf::Keyboard::Up) {
                navigateUp();
            }
            else if (event.key.code == sf::Keyboard::Down) {
                navigateDown();
            }
            else if (event.key.code == sf::Keyboard::Return || 
                     event.key.code == sf::Keyboard::Space) {
            }
            else if (event.key.code == sf::Keyboard::Escape) {
            }
        }
    }
}

void ModeSelection::update(sf::Vector2f mousePos) {
    updateButtonStates(mousePos);
}

void ModeSelection::draw() {
    window.clear();
    
    window.draw(backgroundSprite);
    
    sf::RectangleShape overlay(sf::Vector2f(800, 600));
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
    std::cout << "[ModeSelection] Écran de sélection lancé\n";

    while (window.isOpen()) {
        sf::Event event;
        while (window.pollEvent(event)) {
            switch (event.type) {
                case sf::Event::Closed:
                    window.close();
                    return GameMode::Back;

                case sf::Event::Resized: {
                    sf::FloatRect visibleArea(0, 0, event.size.width, event.size.height);
                    window.setView(sf::View(visibleArea));
                    break;
                }

                case sf::Event::LostFocus:
                case sf::Event::GainedFocus:
                    break;

                case sf::Event::KeyPressed:
                    if (event.key.code == sf::Keyboard::Up)
                        navigateUp();
                    else if (event.key.code == sf::Keyboard::Down)
                        navigateDown();
                    else if (event.key.code == sf::Keyboard::Return || event.key.code == sf::Keyboard::Space) {
                        GameMode selected = selectCurrent();
                        std::cout << "[ModeSelection] Sélection validée (touche)\n";
                        return selected;
                    } else if (event.key.code == sf::Keyboard::Escape) {
                        std::cout << "[ModeSelection] Retour au menu (Echap)\n";
                        return GameMode::Back;
                    }
                    break;

                case sf::Event::MouseButtonPressed:
                    if (event.mouseButton.button == sf::Mouse::Left) {
                        sf::Vector2f mousePos = window.mapPixelToCoords({event.mouseButton.x, event.mouseButton.y});
                        for (const auto& btn : buttons) {
                            if (btn.shape.getGlobalBounds().contains(mousePos)) {
                                std::cout << "[ModeSelection] Clic sur bouton\n";
                                return btn.action;
                            }
                        }
                    }
                    break;

                default:
                    break;
            }
        }

        sf::Vector2i mousePixelPos = sf::Mouse::getPosition(window);
        sf::Vector2f mousePos = window.mapPixelToCoords(mousePixelPos);
        update(mousePos);

        window.clear(sf::Color(10, 10, 30));
        draw();
    }

    return GameMode::Back;
}
