#ifndef MENU_HPP
#define MENU_HPP

#include <SFML/Graphics.hpp>
#include <string>
#include <vector>

enum class MenuState { 
    Main, NewGame, Continue, Options, HighScore, Credits, Quit 
};

class Button {
public:
    sf::RectangleShape rect;
    sf::Text text;
    const sf::Font* font;
    bool isHovered = false;
    bool isClicked = false;
    float originalX = 0.f;
    float originalY = 0.f;
    std::string action;

    Button(const sf::Font& f, const std::string& label, const std::string& act, 
           float x, float y, float width, float height);
    void update(const sf::Vector2f& mousePos, bool isSelected = false);
    void draw(sf::RenderTarget& target) const;
    bool onClick();
};

class IconButton {
public:
    sf::CircleShape circle;
    std::vector<sf::RectangleShape> shapes;  // Pour dessiner l'icône
    bool isHovered = false;
    bool isToggled = false;
    float centerX = 0.f;
    float centerY = 0.f;
    std::string type;  // "mute" ou "stop"

    IconButton();
    IconButton(const std::string& iconType, float x, float y, float radius);
    void update(const sf::Vector2f& mousePos);
    void draw(sf::RenderTarget& target) const;
    void onClick();
    
private:
    void createMuteIcon(float x, float y, float size);
    void createStopIcon(float x, float y, float size);
};

class Menu {
public:
    std::vector<Button> buttons;
    IconButton stopButton;
    IconButton muteButton;
    sf::Font font;
    MenuState state = MenuState::Main;
    int selectedButtonIndex = 0;  // Index du bouton sélectionné avec le clavier

    Menu(const sf::RenderWindow& window);
    void update(const sf::Vector2f& mousePos);
    void draw(sf::RenderTarget& target) const;
    MenuState handleClick(const sf::Vector2f& mousePos);
    bool shouldQuit() const { return state == MenuState::Quit; }
    
    // Navigation clavier
    void navigateUp();
    void navigateDown();
    MenuState selectCurrentButton();
};

#endif // MENU_HPP