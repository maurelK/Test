#include "Menu.hpp"
#include <iostream>
#include "Background.hpp"
#include "Logo.hpp"

extern float gridOffsetX;
extern float gridOffsetY;
extern float gridSpeed;

Menu::Menu(const sf::RenderWindow& window)
    : logo(window)
{
    if (!font.loadFromFile("assets/logo_font.ttf")) {
        if (!font.loadFromFile("logo_font.ttf")) {
            std::cerr << "[Menu] Erreur: impossible de charger la police" << std::endl;
        }
    }

    // === Fond animé ===
    createStars(stars, 60, window);

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

    std::cout << "[Menu] Initialisé avec fond animé et logo" << std::endl;
}

void Menu::update(const sf::Vector2f& mousePos) {
    float dt = 0.016f; // approximativement 60 FPS

    // === Animation du fond ===
    gridOffsetX += gridSpeed * dt;
    gridOffsetY += gridSpeed * dt;

    for (auto& star : stars)
        star.update(dt);

    updateParticles(particles, particleClock, dt);

    logo.update(dt);

    // === Interface ===
    for (size_t i = 0; i < buttons.size(); ++i)
        buttons[i].update(mousePos, i == static_cast<size_t>(selectedButtonIndex));

    stopButton.update(mousePos);
    muteButton.update(mousePos);
}

void Menu::draw(sf::RenderTarget& target) const {
    // === FOND ===
    drawGrid(target, gridOffsetX, gridOffsetY);
    for (const auto& star : stars)
        star.draw(target);
    for (const auto& particle : particles)
        particle.draw(target);

    // === INTERFACE ===
    logo.draw(target);

    for (const auto& btn : buttons)
        btn.draw(target);

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

void Menu::navigateUp() {
    if (selectedButtonIndex > 0)
        selectedButtonIndex--;
    else
        selectedButtonIndex = static_cast<int>(buttons.size()) - 1;
}

void Menu::navigateDown() {
    if (selectedButtonIndex < static_cast<int>(buttons.size()) - 1)
        selectedButtonIndex++;
    else
        selectedButtonIndex = 0;
}

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

MenuState Menu::run(sf::RenderWindow& window) {
    sf::Clock clock;

    while (window.isOpen()) {
        sf::Event event;
        while (window.pollEvent(event)) {
            if (event.type == sf::Event::Closed)
                return MenuState::Quit;

            if (event.type == sf::Event::KeyPressed) {
                if (event.key.code == sf::Keyboard::Up)
                    navigateUp();
                else if (event.key.code == sf::Keyboard::Down)
                    navigateDown();
                else if (event.key.code == sf::Keyboard::Return || event.key.code == sf::Keyboard::Space)
                    return selectCurrentButton();
                else if (event.key.code == sf::Keyboard::Escape)
                    return MenuState::Quit;
            }

            if (event.type == sf::Event::MouseButtonPressed &&
                event.mouseButton.button == sf::Mouse::Left) {
                sf::Vector2f mousePos = window.mapPixelToCoords(
                    {event.mouseButton.x, event.mouseButton.y});
                return handleClick(mousePos);
            }
        }

        float dt = clock.restart().asSeconds();
        sf::Vector2i mousePixelPos = sf::Mouse::getPosition(window);
        sf::Vector2f mousePos = window.mapPixelToCoords(mousePixelPos);

        update(mousePos);

        window.clear(sf::Color(10, 10, 30));
        draw(window);
        window.display();
    }

    return MenuState::Quit;
}
