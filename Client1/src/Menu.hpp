#ifndef MENU_HPP
#define MENU_HPP

#include <SFML/Graphics.hpp>
#include <string>
#include <vector>
#include "Logo.hpp"
#include "Background.hpp"

// --- États possibles du menu ---
enum class MenuState {
    Main,
    NewGame,
    Continue,
    Options,
    HighScore,
    Credits,
    Quit
};

// --- Classe Bouton principal ---
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

// --- Classe pour les petits boutons (mute, stop) ---
class IconButton {
public:
    sf::CircleShape circle;
    std::vector<sf::RectangleShape> shapes; // formes de l’icône
    bool isHovered = false;
    bool isToggled = false;
    float centerX = 0.f;
    float centerY = 0.f;
    std::string type; // "mute" ou "stop"

    IconButton();
    IconButton(const std::string& iconType, float x, float y, float radius);

    void update(const sf::Vector2f& mousePos);
    void draw(sf::RenderTarget& target) const;
    void onClick();

private:
    void createMuteIcon(float x, float y, float size);
    void createStopIcon(float x, float y, float size);
};

// --- Classe principale du menu ---
class Menu {
public:
    std::vector<Button> buttons;
    IconButton stopButton;
    IconButton muteButton;
    sf::Font font;
    Logo logo;                       // ✅ Logo pulsant "R-TYPE"
    std::vector<Star> stars;         // ✅ Étoiles scintillantes
    std::vector<Particle> particles; // ✅ Particules flottantes
    sf::Clock particleClock;

    MenuState state = MenuState::Main;
    int selectedButtonIndex = 0;

    Menu(const sf::RenderWindow& window);

    // --- Boucle principale ---
    MenuState run(sf::RenderWindow& window);

    // --- Logique du menu ---
    void update(const sf::Vector2f& mousePos);
    void draw(sf::RenderTarget& target) const;
    MenuState handleClick(const sf::Vector2f& mousePos);
    void navigateUp();
    void navigateDown();
    MenuState selectCurrentButton();

    bool shouldQuit() const { return state == MenuState::Quit; }
};

#endif // MENU_HPP
