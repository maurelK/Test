#include "GameClient.hpp"
#include <iostream>
#include <algorithm>
#include "game/Game.hpp"
#include "ModeSelection.hpp"
#include "Lobby.hpp"

GameClient::GameClient(const std::string& user)
: username(user)
, window(sf::VideoMode(800, 600), "R-Type Client", sf::Style::Close)
, player(200.f, 360.f)
, net("127.0.0.1", 9091)
{
    window.setFramerateLimit(60);
    renderer.initBackground(100, window.getSize().x, window.getSize().y);
}

GameClient::~GameClient() {}

void GameClient::runClient() {
    std::cout << "[Client] Démarrage du client avec menu principal\n";

    Menu mainMenu(window);

    while (window.isOpen()) {
        MenuState menuResult = mainMenu.run(window);

        if (menuResult == MenuState::Quit) {
            std::cout << "[Client] Fermeture du jeu.\n";
            window.close();
            break;
        }

        if (menuResult == MenuState::NewGame) {

            ModeSelection modeSelection(window);
            GameMode mode = modeSelection.run();

            if (mode == GameMode::Solo) {
                std::cout << "[Client] Mode solo choisi\n";
                runLocalGame();
            } 
            else if (mode == GameMode::Multiplayer) {
                std::cout << "[Client] Mode multijoueur choisi\n";
                runLobby();
            }
        }
    }
}


void GameClient::runLocalGame() {
    std::cout << "[Client] Lancement du mode solo (Benaya)\n";
    Game soloGame(GameModeType::Solo);
    soloGame.run();
}

void GameClient::runLobby() {
    std::cout << "[Client] Ouverture du lobby multijoueur\n";

    LobbyState result = runLobbyScreen();

    if (result == LobbyState::Back) {
        std::cout << "[Client] Retour au menu de sélection\n";
        return;
    }

    if (result == LobbyState::GameStart) {
        std::cout << "[Client] Le jeu multijoueur démarre !\n";

        if (!initNetworked()) {
            std::cerr << "[Client] Erreur lors de l'initialisation réseau UDP.\n";
            return;
        }

        runMultiplayerGame();
    }
}
LobbyState GameClient::runLobbyScreen() {
    sf::Clock clock;
    Lobby lobby(window);
    LobbyState state = LobbyState::Idle;

    bool startedNetwork = false;

    while (window.isOpen()) {
        sf::Event event;
        while (window.pollEvent(event)) {
            if (event.type == sf::Event::Closed)
                window.close();

            if (event.type == sf::Event::TextEntered)
                lobby.handleTextInput(event.text.unicode);

            if (event.type == sf::Event::KeyPressed) {
                if (event.key.code == sf::Keyboard::BackSpace)
                    lobby.handleBackspace();
            }

            if (event.type == sf::Event::MouseButtonPressed &&
                event.mouseButton.button == sf::Mouse::Left) {
                sf::Vector2f mousePos = window.mapPixelToCoords(
                    {event.mouseButton.x, event.mouseButton.y});
                lobby.handleClick(mousePos);

                if (lobby.getState() == LobbyState::Connecting && !startedNetwork) {
                    startedNetwork = true;
                    lobby.runNetwork(net);
                }
            }
        }

        float dt = clock.restart().asSeconds();
        sf::Vector2i mousePixelPos = sf::Mouse::getPosition(window);
        sf::Vector2f mousePos = window.mapPixelToCoords(mousePixelPos);

        lobby.update(mousePos, dt);

        window.clear(sf::Color(10, 10, 30));
        lobby.draw(window);
        window.display();

        state = lobby.getState();

        if (state == LobbyState::Back)
            return LobbyState::Back;

        if (state == LobbyState::GameStart)
            return LobbyState::GameStart;

        if (state == LobbyState::Error)
            return LobbyState::Back;
    }

    return LobbyState::Back;
}

bool GameClient::initNetworked() {
    if (!net.start()) {
        std::cerr << "[Client] Erreur: impossible de démarrer la connexion UDP.\n";
        return false;
    }

    std::cout << "[Client] Initialisation réseau terminée.\n";
    return true;
}

void GameClient::runMultiplayerGame() {
    std::cout << "[Client] Multiplayer game started.\n";

    while (window.isOpen()) {
        sf::Event ev;
        while (window.pollEvent(ev)) {
            if (ev.type == sf::Event::Closed)
                window.close();
            if (ev.type == sf::Event::KeyPressed && ev.key.code == sf::Keyboard::Escape)
                window.close();
        }

        float dt = frameClock.restart().asSeconds();
        updateNetworked(dt);
        renderNetworked();
    }
}

void GameClient::updateNetworked(float dt) {
    renderer.updateBackground(dt);

    float mx = 0.f, my = 0.f;
    bool shoot = false;

    if (sf::Keyboard::isKeyPressed(sf::Keyboard::Up))    my -= 1.f;
    if (sf::Keyboard::isKeyPressed(sf::Keyboard::Down))  my += 1.f;
    if (sf::Keyboard::isKeyPressed(sf::Keyboard::Left))  mx -= 1.f;
    if (sf::Keyboard::isKeyPressed(sf::Keyboard::Right)) mx += 1.f;

    if (sf::Keyboard::isKeyPressed(sf::Keyboard::Space)) {
        if (bulletCooldownClock.getElapsedTime().asMilliseconds() > 130) {
            shoot = true;
            bulletCooldownClock.restart();
        }
    }

    InputPacket input{};
    input.header.type = PacketType::INPUT;
    input.header.size = sizeof(InputPacket);
    input.player_id = playerId;
    input.move_x = mx;
    input.move_y = my;
    input.shoot  = shoot;
    net.sendInput(input);

    SnapshotPacket snap{};
    if (net.pollSnapshot(snap)) {
        handleSnapshot(snap);
    }

    player.update(dt);
    if (shoot) bullets.emplace_back(player.shoot());
    applyBulletLogic(dt);

    if (enemySpawnClock.getElapsedTime().asSeconds() > 1.6f) {
        spawnEnemy();
        enemySpawnClock.restart();
    }
    for (auto& e : enemies) e.update(dt);

    if (powerUpSpawnClock.getElapsedTime().asSeconds() > 2.5f) {
        spawnPowerUp();
        powerUpSpawnClock.restart();
    }
    for (auto& p : powerUps) p.update(dt);

    handleCollisions();

    enemies.erase(std::remove_if(enemies.begin(), enemies.end(),
        [](const Enemy& e){ return e.isDead() || e.getPosition().x < -80.f; }),
        enemies.end());

    powerUps.erase(std::remove_if(powerUps.begin(), powerUps.end(),
        [](const PowerUp& p){ return p.isOutOfBounds(); }),
        powerUps.end());
}

void GameClient::renderNetworked() {
    window.clear(sf::Color(10, 10, 30));
    renderer.drawBackground(window);

    for (const auto& [id, vis] : worldEntities) {
        sf::CircleShape dot(6.f);
        dot.setOrigin(6.f, 6.f);
        dot.setPosition(vis.x, vis.y);
        dot.setFillColor(sf::Color(255, 200, 0));
        window.draw(dot);
    }

    for (auto& b : bullets) b.draw(window);
    for (auto& e : enemies) e.draw(window);
    for (auto& p : powerUps) p.draw(window);
    player.draw(window);

    window.display();
}


void GameClient::handleSnapshot(const SnapshotPacket& snapshot) {
    worldEntities.clear();
    for (int i = 0; i < snapshot.num_entities; i++) {
        auto e = snapshot.entities[i];
        worldEntities[e.id] = { e.x, e.y };
    }
}

void GameClient::spawnEnemy() {
    float y = 50.f + static_cast<float>(rand() % 500);
    int level = 1 + (rand() % 3);
    enemies.emplace_back(850.f, y, level);
}

void GameClient::spawnPowerUp() {
    float y = 50.f + static_cast<float>(rand() % 500);
    PowerUpType type = static_cast<PowerUpType>(rand() % 3);
    powerUps.emplace_back(850.f, y, type);
}

void GameClient::applyBulletLogic(float dt) {
    for (auto& b : bullets) b.update(dt);
    bullets.erase(std::remove_if(bullets.begin(), bullets.end(),
        [&](const Bullet& B){ return B.isOutOfBounds(static_cast<float>(window.getSize().x)); }),
        bullets.end());
}

void GameClient::handleCollisions() {
    for (auto& b : bullets) {
        sf::FloatRect bb = b.getBounds();
        for (auto& e : enemies) {
            if (bb.intersects(e.getBounds())) {
                e.takeDamage(1);
            }
        }
    }
}
