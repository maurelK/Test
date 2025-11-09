#pragma once
#include <SFML/Graphics.hpp>
#include <string>
#include <vector>
#include "RoundedRectangleShape.hpp"

enum class LobbyListResult {
    None,
    Join,
    Create,
    Back,
    Refresh
};


struct LobbyButton {
    RoundedRectangleShape box;
    RoundedRectangleShape shadow;
    bool hovered = false;
    float hoverAnim = 0.f;
    sf::Text label;
};

struct StaticLobbyItem {
    std::string name;
    int players;
    int capacity;
    bool selected = false;
};

class LobbyListScreen {
public:
    explicit LobbyListScreen(sf::RenderWindow& window);
    ~LobbyListScreen() = default;

    LobbyListResult run();

    int getSelectedIndex() const { return selectedIndex; }
    StaticLobbyItem getSelectedLobby() const;

private:

    bool createHovered = false;
float createAnim = 0.f;

bool refreshHovered = false;
float refreshAnim = 0.f;

bool backHovered = false;
float backAnim = 0.f;


struct Star {
    sf::CircleShape shape;
    float speed;
    float brightness;
    float phase;
    sf::Color color;
};

    sf::RenderWindow& window;
    sf::Font font;
    sf::Text titleText;
    sf::Text statusText;

    std::vector<StaticLobbyItem> lobbies;
    int selectedIndex = -1;

    RoundedRectangleShape btnCreate, btnBack;
    RoundedRectangleShape btnRefresh;
    sf::Text txtRefresh;
    sf::Text txtCreate, txtBack;
    std::vector<LobbyButton> joinButtons;

    sf::Color BG{5,10,25}, CYAN{0,200,255}, DARK{20,30,50}, ORANGE{255,107,53};
    float listTopY = 170.f;
    float rowHeight = 56.f;
    float listWidth = 620.f;

    std::vector<Star> stars;

    void initUI();
    void initStars();
    void updateStars(float dt);
    void drawStars(sf::RenderTarget& target) const;

    void drawList(sf::RenderTarget& target);
    void drawButtons(sf::RenderTarget& target) const;

    void handleMouseMove(sf::Vector2f mp);
    void handleClick(sf::Vector2f mp, LobbyListResult& out);
};
