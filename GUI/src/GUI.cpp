#include "GUI.hpp"
#include <iostream>
#include <sstream>
#include <vector>

GUI::GUI(int port, const std::string &host)
    : window(), cinematic(), loading(), music(), view(window.window), background(), menu(), shouldStartGame(false)
{
    window.create();
    window.window.clear(sf::Color(204, 255, 204)); // Couleur de fond verte claire
    cinematic.initialize();
    loading.initialize();
    music.initialize();
    view.initialize();
    background.initialize();
    networkManager.connectToServer(host, port);

    // Charger les textures
    loadTextures();
}

void GUI::loadTextures()
{
    if (!tileTexture.loadFromFile("asserts/tile_th.png"))
    {
        std::cerr << "Failed to load tile texture" << std::endl;
    }
    resourceTextures.resize(7);
    for (int i = 0; i < 7; ++i)
    {
        if (!resourceTextures[i].loadFromFile("path/to/resource_" + std::to_string(i) + ".png"))
        {
            std::cerr << "Failed to load resource texture " << i << std::endl;
        }
    }
}

float GUI::calculateTileSize() const
{
    const float windowWidth = 1920.0f;
    const float windowHeight = 1080.0f;

    float tileSizeX = windowWidth / gameState.width;
    float tileSizeY = windowHeight / gameState.height;

    return std::min(tileSizeX, tileSizeY);
}

void GUI::drawMap(sf::RenderWindow &window)
{
    float tileSize = calculateTileSize();

    for (int x = 0; x < gameState.width; ++x)
    {
        for (int y = 0; y < gameState.height; ++y)
        {
            const Tile &tile = gameState.map[x][y];
            sf::Sprite tileSprite(tileTexture);
            tileSprite.setScale(tileSize / tileSprite.getLocalBounds().width,
                                tileSize / tileSprite.getLocalBounds().height);
            tileSprite.setPosition(x * tileSize, y * tileSize);
            window.draw(tileSprite);

            // Dessiner les ressources sur la tuile
            for (int i = 0; i < 7; ++i)
            {
                if (tile.resources[i] > 0)
                {
                    sf::Sprite resourceSprite(resourceTextures[i]);
                    resourceSprite.setScale(tileSize / resourceSprite.getLocalBounds().width,
                                            tileSize / resourceSprite.getLocalBounds().height);
                    resourceSprite.setPosition(x * tileSize, y * tileSize);
                    window.draw(resourceSprite);
                }
            }
        }
    }
}

void GUI::run()
{
    cinematic.animate(window, loading);

    while (window.isOpen() && !shouldStartGame)
    {
        sf::Event event;
        while (window.window.pollEvent(event))
        {
            if (event.type == sf::Event::Closed)
            {
                window.destroy();
            }
            menu.handleEvent(event, shouldStartGame);
            view.handleEvent(event);
        }


        background.update(0.1f);
        background.draw(window.window);
        menu.draw(window.window);
        window.display();
    }

    if (shouldStartGame)
    {
        loadGameData();
        gameLoop();
    }
}

void GUI::gameLoop()
{
    while (window.isOpen())
    {
        sf::Event event;
        while (window.window.pollEvent(event))
        {
            if (event.type == sf::Event::Closed)
            {
                window.destroy();
            }
            view.handleEvent(event);
        }

        std::string response = networkManager.receiveMessage();
        if (!response.empty())
        {
            processServerResponse(response);
        }

        window.window.clear(sf::Color(204, 255, 204)); // Effacer avec la couleur de fond
        drawMap(window.window);
        window.window.display();
    }
}

void GUI::loadGameData()
{
    std::cout << "Loading game data..." << std::endl;
    networkManager.sendMessage("GRAPHIC\n");
    networkManager.sendMessage("msz\n");
    networkManager.sendMessage("tna\n");
    networkManager.sendMessage("mct\n");
}

void GUI::processServerResponse(const std::string &response)
{
    if (response.find("msz") == 0)
    {
        handleMapSizeResponse(response);
    }
    else if (response.find("bct") == 0)
    {
        handleTileContentResponse(response);
    }
    else if (response.find("pnw") == 0)
    {
        handlePlayerConnectionResponse(response);
    }
    else if (response.find("tna") == 0)
    {
        handleTeamNameResponse(response);
    }
    else if (response.find("enw") == 0)
    {
        handleEggLaidResponse(response);
    }
    else if (response.find("WELCOME") == 0)
    {
        std::cout << "Server welcomed the client." << std::endl;
    }
    else
    {
        std::cout << "Unknown response: " << response << std::endl;
    }
}

void GUI::handleMapSizeResponse(const std::string &response)
{
    std::istringstream iss(response);
    std::string token;
    iss >> token;
    iss >> gameState.width >> gameState.height;
    std::cout << "Map size: " << gameState.width << "x" << gameState.height << std::endl;
    gameState.map.resize(gameState.width, std::vector<Tile>(gameState.height));
}

void GUI::handleTileContentResponse(const std::string &response)
{
    std::istringstream iss(response);
    std::string token;
    int x, y;
    iss >> token >> x >> y;

    Tile &tile = gameState.map[x][y];
    tile.x = x;
    tile.y = y;
    for (int i = 0; i < 7; ++i)
    {
        iss >> tile.resources[i];
    }
}

void GUI::handlePlayerConnectionResponse(const std::string &response)
{
    std::istringstream iss(response);
    std::string token;
    Player player;
    iss >> token >> player.id >> player.x >> player.y >> player.orientation >> player.level >> player.teamName;
    gameState.players[player.id] = player;
    std::cout << "New player connected: ID " << player.id << " at (" << player.x << ", " << player.y << ")" << std::endl;
}

void GUI::handleTeamNameResponse(const std::string &response)
{
    std::istringstream iss(response);
    std::string token, teamName;
    iss >> token >> teamName;
    Team team;
    team.name = teamName;
    gameState.teams[teamName] = team;
    std::cout << "Team: " << teamName << std::endl;
}

void GUI::handleEggLaidResponse(const std::string &response)
{
    std::istringstream iss(response);
    std::string token;
    int eggId, playerId, x, y;
    iss >> token >> eggId >> playerId >> x >> y;
    std::cout << "Egg laid by player " << playerId << " at (" << x << ", " << y << ")" << std::endl;
}
