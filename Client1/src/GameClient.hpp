#pragma once

#include <SFML/Graphics.hpp>
#include <string>
#include <unordered_map>
#include <vector>
#include "Menu.hpp"
#include "GameRenderer.hpp"
#include "WindowManager.hpp"
#include "game/Player.hpp"
#include "game/Enemy.hpp"
#include "game/Bullet.hpp"
#include "game/PowerUp.hpp"
#include "NetworkClient.hpp"
#include "../Network/protocol.hpp"

enum class LobbyState;
enum class GameMode;

struct NetworkEntityVisual {
    float x, y;
};

class GameClient {
public:
    explicit GameClient(const std::string& username);
    ~GameClient();

    // === POINT D’ENTRÉE PRINCIPAL ===
    void runClient();

    // === MODES DE JEU ===
    void runLocalGame();      // Mode solo
    void runLobby();          // Mode multijoueur → lobby puis partie réseau
    void runMultiplayerGame();// Partie réseau

private:
    // === INITIALISATION ===
    bool initNetworked();
    LobbyState runLobbyScreen();

    // === ATTRIBUTS ===
    std::string username;
    sf::RenderWindow window;

    Player player;
    std::vector<Bullet> bullets;
    std::vector<Enemy> enemies;
    std::vector<PowerUp> powerUps;

    GameRenderer renderer;
    NetworkClient net;

    sf::Clock frameClock;
    sf::Clock bulletCooldownClock;
    sf::Clock enemySpawnClock;
    sf::Clock powerUpSpawnClock;

    int playerId = 0;

    std::unordered_map<int, NetworkEntityVisual> worldEntities;

    // === MÉTHODES INTERNES ===
    void updateNetworked(float dt);
    void renderNetworked();
    void handleSnapshot(const SnapshotPacket& snapshot);

    void spawnEnemy();
    void spawnPowerUp();
    void applyBulletLogic(float dt);
    void handleCollisions();
};
