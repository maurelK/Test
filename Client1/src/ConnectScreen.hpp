#pragma once

#include <SFML/Graphics.hpp>
#include <string>
#include <thread>
#include <atomic>
#include <memory>
#include <vector>
#include "NetworkClient.hpp"
#include "RoundedRectangleShape.hpp"

enum class ConnectState {
    Idle,
    Connecting,
    Connected,
    WaitingStart,
    GameStart,
    Error,
    Back
};

struct ConnectStar {
    sf::CircleShape shape;
    float speed;
};

class TextField {
public:
    RoundedRectangleShape box;
    sf::Text label;
    sf::Text content;
    std::string text;
    bool isActive;
    bool isHovered;
    float cursorTime;

    TextField(const sf::Font& font, const std::string& labelText,
              float centerX, float y, float width, float height);

    void update(const sf::Vector2f& mousePos, float deltaTime);
    void draw(sf::RenderTarget& target) const;
    void handleInput(sf::Uint32 unicode);
    void handleBackspace();
    void setActive(bool active);
    bool contains(const sf::Vector2f& point) const;
};

class ConnectScreenButton {
public:
    RoundedRectangleShape rect;
    sf::Text text;
    bool isHovered;

    ConnectScreenButton(const sf::Font& font, const std::string& label,
                        float centerX, float y, float width, float height);

    void update(const sf::Vector2f& mousePos);
    void draw(sf::RenderTarget& target) const;
    bool isClicked(const sf::Vector2f& mousePos) const;
};

class ConnectScreen {
private:
    sf::Font font;
    sf::RenderWindow& window;

    TextField playerNameField;
    TextField serverIPField;
    TextField serverPortField;

    ConnectScreenButton connectButton;
    ConnectScreenButton backButton;

    sf::Text titleText;
    sf::Text subtitleText;
    sf::Text statusText;

    std::vector<ConnectStar> stars;

    ConnectState state;
    TextField* activeField;

    std::thread networkThread;
    std::atomic<bool> running{false};

    void initStars();
    void updateStars(float dt);
    void drawStars(sf::RenderTarget& target) const;

public:
    explicit ConnectScreen(sf::RenderWindow& window);
    ~ConnectScreen();

    void update(const sf::Vector2f& mousePos, float deltaTime);
    void draw(sf::RenderTarget& target) const;
    void handleClick(const sf::Vector2f& mousePos);
    void handleTextInput(sf::Uint32 unicode);
    void handleBackspace();

    ConnectState getState() const { return state; }
    std::string getPlayerName() const { return playerNameField.text; }
    std::string getServerIP() const { return serverIPField.text; }
    std::string getServerPort() const { return serverPortField.text; }

    void setStatus(const std::string& status, bool isError = false);
    void runNetwork(NetworkClient& client);
};