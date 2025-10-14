#pragma once
#include <SFML/Graphics.hpp>
#include <vector>

enum class GameMode {
    None,        // ✅ utilisé pour éviter les actions involontaires
    Solo,
    Multiplayer,
    Back
};

class ModeSelection {
public:
    explicit ModeSelection(sf::RenderWindow& window);
    ~ModeSelection();

    GameMode run(); // boucle principale (affiche et retourne la sélection)

private:
    struct Button {
        sf::RectangleShape shape;
        sf::Text text;
        GameMode action;
        bool isHovered = false;
    };

    sf::RenderWindow& window;
    sf::Font font;

    sf::Texture backgroundTexture;
    sf::Sprite backgroundSprite;

    sf::Text titleText;
    sf::Text instructionText;

    std::vector<Button> buttons;
    int selectedIndex;

    GameMode pendingSelection; // ✅ stocke la sélection avant validation

    // === Initialisation ===
    void initBackground();
    void initTitle();
    void initButtons();
    void initInstructions();

    // === Logique ===
    void handleEvents();
    void update(sf::Vector2f mousePos);
    void draw();

    void navigateUp();
    void navigateDown();
    GameMode selectCurrent();
    void updateButtonStates(sf::Vector2f mousePos);
};
