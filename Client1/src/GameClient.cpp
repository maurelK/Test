#include "GameClient.hpp"
#include <iostream>
#include <algorithm>
#include "game/Game.hpp"
#include "ModeSelection.hpp"
#include "ConnectScreen.hpp"
#include "LobbyListScreen.hpp"

GameClient::GameClient(const std::string& user)
: username(user)
, window(sf::VideoMode(800, 600), "R-Type Client", sf::Style::Close)
, player(200.f, 360.f)
, player2(200.f, 460.f)
, net("127.0.0.1", 9091)
{
    window.setFramerateLimit(60);
    renderer.initBackground(100, window.getSize().x, window.getSize().y);

   /* if (!backgroundMusic.openFromFile("assets/background.ogg")) {
        std::cerr << "[Audio] Erreur : impossible de charger assets/background.wav\n";
    } else {
        backgroundMusic.setLoop(true);
        backgroundMusic.setVolume(40.f);
        backgroundMusic.play();
        std::cout << "[Audio] Musique de fond lancée.\n";
    }*/
}

GameClient::~GameClient() {
    if (backgroundMusic.getStatus() == sf::Music::Playing)
        backgroundMusic.stop();
}

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
                std::cout << "[Client] Mode local 2 joueurs choisi\n";
                runLocalTwoPlayers();
            } 
            else if (mode == GameMode::Multiplayer) {
                std::cout << "[Client] Mode multijoueur choisi\n";
                ConnectState result = runConnectScreen();
                
                if(result == ConnectState::GameStart) {
                    std::cout << "[Client] Connexion réussi, ouverture de la liste des lobby\n";

                    LobbyListResult lobbyChoice = runLobbyListScreen();
                    if(lobbyChoice == LobbyListResult::Back) {
                        std::cout << "[Client] Retour au menu précédent depuis le lobby list.\n";
                        continue;
                    }
                    if(lobbyChoice == LobbyListResult::Create) {
                        std::cout << "[Client] Création d'un nouveau lobby...\n";
                    }
                    if(lobbyChoice == LobbyListResult::Join) {
                        std::cout << "[Client] Rejoint un lobby existant.\n";
                    }

                    if(!initNetworked()) {
                        std::cerr <<"[Client] Erreur lors  de l'initialisation réseau UDP.\n";
                        continue;
                    }
                    runMultiplayerGame();
                }
            }
        }
    }
}

void GameClient::runLocalGame() {
    std::cout << "[Client] Lancement du mode solo (Benaya)\n";
    Game soloGame(GameModeType::Solo);
    soloGame.run();
}
/*
void GameClient::runLobby() {
    std::cout << "[Client] Ouverture du lobby multijoueur\n";

    State result = runLobbyScreen();

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
*/
ConnectState GameClient::runConnectScreen() {
    sf::Clock clock;
    ConnectScreen connect(window);
    ConnectState state = ConnectState::Idle;

    bool startedNetwork = false;

    while (window.isOpen()) {
        sf::Event event;
        while (window.pollEvent(event)) {
            if (event.type == sf::Event::Closed)
                window.close();

            if (event.type == sf::Event::TextEntered)
                connect.handleTextInput(event.text.unicode);

            if (event.type == sf::Event::KeyPressed) {
                if (event.key.code == sf::Keyboard::BackSpace)
                    connect.handleBackspace();
            }

            if (event.type == sf::Event::MouseButtonPressed &&
                event.mouseButton.button == sf::Mouse::Left) {
                sf::Vector2f mousePos = window.mapPixelToCoords(
                    {event.mouseButton.x, event.mouseButton.y});
                connect.handleClick(mousePos);

                if (connect.getState() == ConnectState::Connecting && !startedNetwork) {
                    startedNetwork = true;
                    connect.runNetwork(net);
                }
            }
        }

        float dt = clock.restart().asSeconds();
        sf::Vector2i mousePixelPos = sf::Mouse::getPosition(window);
        sf::Vector2f mousePos = window.mapPixelToCoords(mousePixelPos);

        connect.update(mousePos, dt);

        window.clear(sf::Color(10, 10, 30));
        connect.draw(window);
        window.display();

        state = connect.getState();

        if (state == ConnectState::Back)
            return ConnectState::Back;

        if (state == ConnectState::GameStart)
            return ConnectState::GameStart;

        if (state == ConnectState::Error)
            return ConnectState::Back;
    }

    return ConnectState::Back;
}

LobbyListResult GameClient::runLobbyListScreen()
{
    LobbyListScreen list(window);
    LobbyListResult result = list.run();

    if (result == LobbyListResult::Join) {
        auto lobby = list.getSelectedLobby();
        std::cout << "[Client] Joueur veut rejoindre le lobby : " 
                  << lobby.name << " (" << lobby.players << "/" << lobby.capacity << ")\n";
    }
    else if (result == LobbyListResult::Create) {
        std::cout << "[Client] Joueur veut créer un nouveau lobby.\n";
    }
    else if (result == LobbyListResult::Back) {
        std::cout << "[Client] Retour au menu précédent.\n";
    }

    return result;
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
    std::cout << "[Client] Lancement du mode multijoueur...\n";


    playerId = net.getPlayerId();
    std::cout << "[Client] Player ID attribué : " << playerId << std::endl;

    sf::Clock clock;
    while (window.isOpen()) {
        float dt = clock.restart().asSeconds();

        sf::Event event;
        while (window.pollEvent(event)) {
            if (event.type == sf::Event::Closed) {
                window.close();
            }
        }

        updateNetworked(dt);

        renderNetworked();
    }

    std::cout << "[Client] Fin du mode multijoueur.\n";
}

void GameClient::runLocalTwoPlayers() {
    std::cout << "[Client] Mode 2 joueurs sur le même écran\n";

    sf::Clock clock;
    static sf::Clock shootClock1, shootClock2;
    const float speed = 300.f;

    auto clampToWindow = [&](sf::Vector2f p) {
        float w = static_cast<float>(window.getSize().x);
        float h = static_cast<float>(window.getSize().y);
        if (p.x < 0.f) p.x = 0.f;
        if (p.y < 0.f) p.y = 0.f;
        if (p.x > w - 40.f)  p.x = w - 40.f;
        if (p.y > h - 40.f)  p.y = h - 40.f;
        return p;
    };

    while (window.isOpen()) {
        sf::Event event;
        while (window.pollEvent(event)) {
            if (event.type == sf::Event::Closed)
                window.close();
        }

        float dt = clock.restart().asSeconds();

        sf::Vector2f dir1(0.f, 0.f);
        bool shoot1 = false;

        if (sf::Keyboard::isKeyPressed(sf::Keyboard::Up))    dir1.y -= 1.f;
        if (sf::Keyboard::isKeyPressed(sf::Keyboard::Down))  dir1.y += 1.f;
        if (sf::Keyboard::isKeyPressed(sf::Keyboard::Left))  dir1.x -= 1.f;
        if (sf::Keyboard::isKeyPressed(sf::Keyboard::Right)) dir1.x += 1.f;

        if (sf::Keyboard::isKeyPressed(sf::Keyboard::Space)) {
            if (shootClock1.getElapsedTime().asMilliseconds() > 200) {
                shoot1 = true;
                shootClock1.restart();
            }
        }

        sf::Vector2f dir2(0.f, 0.f);
        bool shoot2 = false;

        if (sf::Keyboard::isKeyPressed(sf::Keyboard::W)) dir2.y -= 1.f;
        if (sf::Keyboard::isKeyPressed(sf::Keyboard::S)) dir2.y += 1.f;
        if (sf::Keyboard::isKeyPressed(sf::Keyboard::A)) dir2.x -= 1.f;
        if (sf::Keyboard::isKeyPressed(sf::Keyboard::D)) dir2.x += 1.f;

        if (sf::Keyboard::isKeyPressed(sf::Keyboard::LShift)) {
            if (shootClock2.getElapsedTime().asMilliseconds() > 200) {
                shoot2 = true;
                shootClock2.restart();
            }
        }

        sf::Vector2f p1 = player.getPosition();
        sf::Vector2f p2 = player2.getPosition();

        p1 += dir1 * speed * dt;
        p2 += dir2 * speed * dt;

        p1 = clampToWindow(p1);
        p2 = clampToWindow(p2);

        player.setPosition(p1.x, p1.y);
        player2.setPosition(p2.x, p2.y);

        if (shoot1)
            bullets.emplace_back(player.shoot());
        if (shoot2)
            bullets2.emplace_back(player2.shoot());

        for (auto& b : bullets)
            b.update(dt);
        for (auto& b : bullets2)
            b.update(dt);

        bullets.erase(std::remove_if(bullets.begin(), bullets.end(),
            [&](const Bullet& B){ return B.isOutOfBounds((float)window.getSize().x); }),
            bullets.end());

        bullets2.erase(std::remove_if(bullets2.begin(), bullets2.end(),
            [&](const Bullet& B){ return B.isOutOfBounds((float)window.getSize().x); }),
            bullets2.end());

        window.clear(sf::Color(10, 10, 30));
        renderer.drawBackground(window);

        for (auto& b : bullets)
            b.draw(window);
        for (auto& b : bullets2)
            b.draw(window);

        player.draw(window);
        player2.draw(window);

        window.display();
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
    for (auto& p : powerUps) p.update(dt);

    handleCollisions();

    enemies.erase(std::remove_if(enemies.begin(), enemies.end(),
        [](const Enemy& e){ return e.isDead() || e.getPosition().x < -80.f; }),
        enemies.end());

    powerUps.erase(std::remove_if(powerUps.begin(), powerUps.end(),
        [](const PowerUp& p){ return p.isOutOfBounds() || p.isCollected(); }),
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
    
    multiplayerHUD.draw(window);
    window.display();
}

void GameClient::handleSnapshot(const SnapshotPacket& snapshot) {
    worldEntities.clear();
    for (int i = 0; i < snapshot.num_entities; i++) {
        auto e = snapshot.entities[i];
        worldEntities[e.id] = { e.x, e.y };
    }
    
    currentPlayerStats.clear();
    for (int i = 0; i < snapshot.num_players; i++) {
        currentPlayerStats.push_back(snapshot.player_stats[i]);
        
        if (snapshot.player_stats[i].player_id == static_cast<uint32_t>(playerId)) {
            playerScore = snapshot.player_stats[i].score;
            playerLives = snapshot.player_stats[i].lives;
            playerKills = snapshot.player_stats[i].enemies_killed;
        }
    }
    
    multiplayerHUD.updateStats(currentPlayerStats);
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
                if (e.isDead()) {
                    playerKills++;
                    playerScore += 100;

                    if (rand() % 100 < 30) {
                        sf::Vector2f enemyPos = e.getPosition();
                        PowerUpType type = (rand() % 100 < 85) ? PowerUpType::Life : PowerUpType::Score;
                        powerUps.emplace_back(enemyPos.x, enemyPos.y, type);
                        std::cout << "[Client] Power-up généré!\n";
                    }
                }
            }
        }
    }

    sf::FloatRect playerBounds = player.getBounds();
    for (auto& e : enemies) {
        if (playerBounds.intersects(e.getBounds())) {
            playerLives--;
            e.takeDamage(999);
            if (playerLives <= 0) {
                std::cout << "[Client] Game Over!\n";
            }
        }
    }

    for (auto& p : powerUps) {
        if (playerBounds.intersects(p.getBounds())) {
            if (p.getType() == PowerUpType::Life) {
                playerLives++;
                std::cout << "[Client] Vie récupérée! Vies: " << playerLives << "\n";
            } else if (p.getType() == PowerUpType::Score) {
                playerScore += 500;
                std::cout << "[Client] Bonus de score! Score: " << playerScore << "\n";
            }
            p.collect();
        }
    }
}