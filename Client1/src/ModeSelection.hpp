#pragma once
#include <SFML/Graphics.hpp>
#include <vector>

enum class GameMode {
    Solo,
    Multiplayer,
    Back
};

class ModeSelection {
public:
    ModeSelection(sf::RenderWindow& window);
    ~ModeSelection();
    
    GameMode run();
    
private:
    struct Button {
        sf::RectangleShape shape;
        sf::Text text;
        GameMode action;
        bool isHovered;
    };
    
    sf::RenderWindow& window;
    sf::Font font;
    sf::Texture backgroundTexture;
    sf::Sprite backgroundSprite;
    sf::Text titleText;
    sf::Text instructionText;
    
    std::vector<Button> buttons;
    int selectedIndex;
    
    void initBackground();
    void initTitle();
    void initButtons();
    void initInstructions();
    void handleEvents();
    void update(sf::Vector2f mousePos);
    void draw();
    void navigateUp();
    void navigateDown();
    GameMode selectCurrent();
    void updateButtonStates(sf::Vector2f mousePos);
};
