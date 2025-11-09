#pragma once

#include <SFML/Graphics.hpp>
#include <string>
#include <unordered_map>
#include <vector>
#include "Menu.hpp"
#include <SFML/Audio.hpp>
#include "GameRenderer.hpp"
#include "WindowManager.hpp"
#include "game/Player.hpp"
#include "game/Enemy.hpp"
#include "game/Bullet.hpp"
#include "game/PowerUp.hpp"
#include "NetworkClient.hpp"
#include "MultiplayerHUD.hpp"
#include "../../Network/protocol.hpp"
#include "LobbyListScreen.hpp"

enum class ConnectState;
enum class GameMode;

struct NetworkEntityVisual {
    float x, y;
};

class GameClient {
public:
    explicit GameClient(const std::string& username);
    ~GameClient();

    void runClient();

    void runLocalGame(); 
    void runLobby();   
    void runMultiplayerGame();

    void runLocalTwoPlayers();

private:
    bool initNetworked();
    ConnectState runConnectScreen();
    LobbyListResult runLobbyListScreen();

    std::string username;
    sf::RenderWindow window;

    sf::Music backgroundMusic;

    Player player;
    Player player2;

    std::vector<Bullet> bullets;
    std::vector<Bullet>bullets2;
    std::vector<Enemy> enemies;
    std::vector<PowerUp> powerUps;

    GameRenderer renderer;
    NetworkClient net;
    MultiplayerHUD multiplayerHUD;

    sf::Clock frameClock;
    sf::Clock bulletCooldownClock;
    sf::Clock enemySpawnClock;
    sf::Clock powerUpSpawnClock;

    int playerId = 0;
    int playerScore = 0;
    int playerLives = 3;
    int playerKills = 0;

    std::unordered_map<int, NetworkEntityVisual> worldEntities;
    std::vector<PlayerStats> currentPlayerStats;

    // === MÉTHODES INTERNES ===
    void updateNetworked(float dt);
    void renderNetworked();
    void handleSnapshot(const SnapshotPacket& snapshot);

    void spawnEnemy();
    void spawnPowerUp();
    void applyBulletLogic(float dt);
    void handleCollisions();
};
