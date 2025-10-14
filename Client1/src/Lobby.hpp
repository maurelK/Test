#ifndef LOBBY_HPP
#define LOBBY_HPP

#include <SFML/Graphics.hpp>
#include <string>

enum class LobbyState {
    Idle,
    Connecting,
    Connected,
    Error,
    Back
};

class TextField {
public:
    sf::RectangleShape box;
    sf::Text label;
    sf::Text content;
    std::string text;
    bool isActive;
    bool isHovered;
    float cursorTime;
    
    TextField(const sf::Font& font, const std::string& labelText, 
              float x, float y, float width, float height);
    
    void update(const sf::Vector2f& mousePos, float deltaTime);
    void draw(sf::RenderTarget& target) const;
    void handleInput(sf::Uint32 unicode);
    void handleBackspace();
    void setActive(bool active);
    bool contains(const sf::Vector2f& point) const;
};

class LobbyButton {
public:
    sf::RectangleShape rect;
    sf::Text text;
    bool isHovered;
    
    LobbyButton(const sf::Font& font, const std::string& label,
                float x, float y, float width, float height);
    
    void update(const sf::Vector2f& mousePos);
    void draw(sf::RenderTarget& target) const;
    bool isClicked(const sf::Vector2f& mousePos) const;
};

class Lobby {
private:
    sf::Font font;
    sf::Texture backgroundTexture;
    sf::Sprite backgroundSprite;
    
    TextField playerNameField;
    TextField serverIPField;
    TextField serverPortField;
    
    LobbyButton connectButton;
    LobbyButton backButton;
    
    sf::Text titleText;
    sf::Text statusText;
    
    LobbyState state;
    TextField* activeField;
    
public:
    Lobby(const sf::RenderWindow& window);
    
    void update(const sf::Vector2f& mousePos, float deltaTime);
    void draw(sf::RenderTarget& target) const;
    void handleClick(const sf::Vector2f& mousePos);
    void handleTextInput(sf::Uint32 unicode);
    void handleBackspace();
    
    LobbyState getState() const { return state; }
    
    std::string getPlayerName() const { return playerNameField.text; }
    std::string getServerIP() const { return serverIPField.text; }
    std::string getServerPort() const { return serverPortField.text; }
    
    void setStatus(const std::string& status, bool isError = false);
};

#endif // LOBBY_HPP
