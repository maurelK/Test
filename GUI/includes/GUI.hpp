#ifndef GUI_HPP
    #define GUI_HPP
    #include "Window.hpp"
    #include "Gamestate.hpp"
    #include "Cinematic.hpp"
    #include "Loading.hpp"
    #include "Music.hpp"
    #include "View.hpp"
    #include "Background.hpp"
    #include "BoolMenu.hpp"
    #include "NetworkManager.hpp"
    #include "Menu.hpp"
    #include <iostream>
    #include <sstream>
    #include <vector>

class GUI {
public:
    GUI(int port, const std::string &host);
    void run();

private:
    void processServerResponse(const std::string &response);
    void handleMapSizeResponse(const std::string &response);
    void handleTileContentResponse(const std::string &response);
    void handlePlayerConnectionResponse(const std::string &response);
    void handleTeamNameResponse(const std::string &response);
    void handleEggLaidResponse(const std::string &response);
    void loadGameData();
    void gameLoop();
    void drawMap(sf::RenderWindow& window);
    float calculateTileSize() const;
    void loadTextures();

    Window window;
    Cinematic cinematic;
    Loading loading;
    Music music;
    View view;
    Background background;
    Menu menu;
    NetworkManager networkManager;
    GameState gameState;
    bool shouldStartGame;
    sf::Texture tileTexture;
    std::vector<sf::Texture> resourceTextures;
};

#endif
