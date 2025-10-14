#include "GameClient.hpp"
#include "TestGame.hpp"
#include "../Network/protocol.hpp"
#include "InputSystem.hpp"
#include "game/RenderSystem.hpp"
#include <iostream>
#include <thread>
#include <chrono>
#include <SFML/Window.hpp>
#include <SFML/Graphics.hpp>
#include <vector>
#include "ModeSelection.hpp"
#include "Lobby.hpp"
#include "NetworkClient.hpp"

GameClient::GameClient(const std::string& user)
    : username(user), playerId(1), menu(nullptr), inMenu(true),
      m_network("127.0.0.1", 9091) {}

GameClient::~GameClient() {
    if (menu) {
        delete menu;
        menu = nullptr;
    }
}

bool GameClient::init() {
    std::cout << "[Client] Initialisation pour l'utilisateur: " << username << "\n";

    if (!m_network.start()) {
        std::cerr << "[Client] Erreur: impossible de démarrer le réseau\n";
        return false;
    }

    ecs.init();
    ecs.registerComponent<Position>();
    ecs.registerComponent<Velocity>();
    ecs.registerComponent<Sprite>();

    inputSystem = ecs.registerSystem<InputSystem>(ecs, m_network);
    Signature sigInput;
    sigInput.set(ecs.getComponentType<Velocity>());
    ecs.setSystemSignature<InputSystem>(sigInput);

    sf::RenderWindow dummyWindow;
    renderSystem = ecs.registerSystem<RenderSystem>(ecs, dummyWindow);
    Signature sigRender;
    sigRender.set(ecs.getComponentType<Position>());
    sigRender.set(ecs.getComponentType<Sprite>());
    ecs.setSystemSignature<RenderSystem>(sigRender);

    playerEntity = ecs.createEntity();
    ecs.addComponent(playerEntity, Position{400.f, 300.f});
    ecs.addComponent(playerEntity, Velocity{0.f, 0.f});

    std::cout << "[Client] ECS initialisé\n";
    return true;
}

void GameClient::runMenu() {
    sf::RenderWindow window(sf::VideoMode(800, 600), "R-Type - Menu Principal");
    window.setFramerateLimit(60);

    menu = new Menu(window);
    Logo logo(window);
    std::vector<Star> stars;
    createStars(stars, 100, window);
    std::vector<Particle> particles;
    sf::Clock clock;
    sf::Clock particleClock;

    std::cout << "[Client] Menu lancé - Flèches pour naviguer, Entrée pour sélectionner\n";

    while (window.isOpen() && inMenu) {
        float deltaTime = clock.restart().asSeconds();
        sf::Event event;

        while (window.pollEvent(event)) {
            switch (event.type) {
                case sf::Event::Closed:
                    window.close();
                    inMenu = false;
                    return;

                case sf::Event::Resized: {
                    sf::FloatRect visibleArea(0, 0, event.size.width, event.size.height);
                    window.setView(sf::View(visibleArea));
                    break;
                }

                case sf::Event::KeyPressed:
                    if (event.key.code == sf::Keyboard::Up)
                        menu->navigateUp();
                    else if (event.key.code == sf::Keyboard::Down)
                        menu->navigateDown();
                    else if (event.key.code == sf::Keyboard::Return || event.key.code == sf::Keyboard::Space) {
                        MenuState newState = menu->selectCurrentButton();

                        //Nouvelle Partie
                        if (newState == MenuState::NewGame) {
                            std::cout << "[Menu] Ouverture de la sélection du mode...\n";
                            window.close();
                            sf::sleep(sf::milliseconds(100));

                            sf::RenderWindow modeWindow(sf::VideoMode(800, 600), "Sélection du mode");
                            ModeSelection modeSelection(modeWindow);
                            GameMode mode = modeSelection.run();

                            // Mode SOLO
                            if (mode == GameMode::Solo) {
                                std::cout << "[ModeSelection] Mode solo choisi.\n";
                                TestGame soloGame;
                                soloGame.run();
                            }

                            // Mode MULTI
                            else if (mode == GameMode::Multiplayer) {
                                std::cout << "[ModeSelection] Mode multijoueur choisi.\n";

                                sf::RenderWindow lobbyWindow(sf::VideoMode(800, 600), "Lobby multijoueur");
                                Lobby lobby(lobbyWindow);
                                sf::Clock lobbyClock;
                                bool connected = false;

                                NetworkClient client("127.0.0.1", 9091);
                                client.start();

                                std::cout << "[Lobby] Ouverture du lobby multijoueur\n";

                                while (lobbyWindow.isOpen()) {
                                    sf::Event lobbyEvent;
                                    while (lobbyWindow.pollEvent(lobbyEvent)) {
                                        switch (lobbyEvent.type) {
                                            case sf::Event::Closed:
                                                lobbyWindow.close();
                                                break;

                                            case sf::Event::Resized: {
                                                sf::FloatRect visibleArea(0, 0, lobbyEvent.size.width, lobbyEvent.size.height);
                                                lobbyWindow.setView(sf::View(visibleArea));
                                                break;
                                            }

                                            case sf::Event::TextEntered:
                                                lobby.handleTextInput(lobbyEvent.text.unicode);
                                                break;

                                            case sf::Event::KeyPressed:
                                                if (lobbyEvent.key.code == sf::Keyboard::BackSpace)
                                                    lobby.handleBackspace();
                                                break;

                                            case sf::Event::MouseButtonPressed:
                                                if (lobbyEvent.mouseButton.button == sf::Mouse::Left) {
                                                    sf::Vector2i mousePixelPos = sf::Mouse::getPosition(lobbyWindow);
                                                    sf::Vector2f mousePos = lobbyWindow.mapPixelToCoords(mousePixelPos);
                                                    lobby.handleClick(mousePos);
                                                }
                                                break;
                                        }
                                    }

                                    float dt = lobbyClock.restart().asSeconds();
                                    sf::Vector2i mousePixelPos = sf::Mouse::getPosition(lobbyWindow);
                                    sf::Vector2f mousePos = lobbyWindow.mapPixelToCoords(mousePixelPos);
                                    lobby.update(mousePos, dt);

                                    lobbyWindow.clear(sf::Color(10, 10, 30));
                                    lobby.draw(lobbyWindow);
                                    lobbyWindow.display();

                                    // Tentative de connexion
                                    if (lobby.getState() == LobbyState::Connecting && !connected) {
                                        std::string username = lobby.getPlayerName();

                                        if (client.sendLogin(username)) {
                                            std::cout << "[Lobby]  Connexion réussie au serveur TCP.\n";
                                            lobby.setStatus("Connexion réussie !", false);
                                            connected = true;
                                        } else {
                                            std::cerr << "[Lobby]  ACK non reçu.\n";
                                            lobby.setStatus("Erreur: ACK non reçu", true);
                                        }
                                    }

                                    if (connected) {
                                        lobby.setStatus("Connecté au serveur. En attente d'autres joueurs...", false);
                                    }

                                    // Retour
                                    if (lobby.getState() == LobbyState::Back) {
                                        std::cout << "[Lobby] Retour au menu principal.\n";
                                        lobbyWindow.close();
                                        client.stop();
                                        break;
                                    }
                                }
                            }

                            // Retour arrière
                            else {
                                std::cout << "[ModeSelection] Retour au menu principal.\n";
                                sf::sleep(sf::milliseconds(100));
                                window.create(sf::VideoMode(800, 600), "R-Type - Menu Principal");
                                inMenu = true;
                                return;
                            }
                        }

                        //Quitter
                        else if (newState == MenuState::Quit) {
                            std::cout << "[Menu] Fermeture du jeu...\n";
                            window.close();
                            inMenu = false;
                            return;
                        }
                    }
                    break;

                default:
                    break;
            }
        }

        // Rendu principal
        sf::Vector2i mousePixelPos = sf::Mouse::getPosition(window);
        sf::Vector2f mousePos = window.mapPixelToCoords(mousePixelPos);
        menu->update(mousePos);

        logo.update(deltaTime);
        for (auto& star : stars) star.update(deltaTime);
        updateParticles(particles, particleClock, deltaTime);

        window.clear(sf::Color(10, 10, 30));
        drawGrid(window, gridOffsetX, gridOffsetY);
        for (auto& star : stars) star.draw(window);
        for (auto& p : particles) p.draw(window);
        logo.draw(window);
        menu->draw(window);
        window.display();
    }
}

void GameClient::shutdown() {
    std::cout << "[Client] Arrêt du client...\n";
    m_network.stop();
}

void GameClient::run() {
    runMenu();

    if (!inMenu && menu && menu->state != MenuState::NewGame) {
        std::cout << "[Client] Fermeture du jeu\n";
        return;
    }

    sf::RenderWindow window(sf::VideoMode(800, 600), "R-Type - Jeu");
    window.setFramerateLimit(60);

    std::cout << "[Client] Jeu lancé !\n";

    renderSystem = ecs.registerSystem<RenderSystem>(ecs, window);

    sf::Clock clock;
    while (window.isOpen()) {
        sf::Event event;
        while (window.pollEvent(event)) {
            if (event.type == sf::Event::Closed)
                window.close();
        }

        float dt = clock.restart().asSeconds();

        inputSystem->update(dt);
        ecs.update(dt);

        SnapshotPacket snap{};
        if (m_network.pollSnapshot(snap))
            handleSnapshot(snap);

        window.clear(sf::Color::Black);
        renderSystem->update(dt);
        window.display();
    }

    shutdown();
}

void GameClient::handleSnapshot(const SnapshotPacket& snapshot) {
    worldEntities.clear();

    for (int i = 0; i < snapshot.num_entities; i++) {
        auto e = snapshot.entities[i];
        worldEntities[e.id] = {e.x, e.y};
    }

    std::cout << "[Client] Snapshot reçu: " << snapshot.num_entities << " entités" << std::endl;
}
