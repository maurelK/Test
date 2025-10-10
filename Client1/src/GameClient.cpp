#include "GameClient.hpp"
#include "TestGame.hpp"
#include "../Network/protocol.hpp"
#include <iostream>
#include <thread>
#include <chrono>
#include <SFML/Window.hpp>
#include <SFML/Graphics.hpp>
#include <vector>

GameClient::GameClient(const std::string& user)
    : username(user), playerId(1),menu(nullptr), inMenu(true),
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
    return true;
}

void GameClient::shutdown() {
    std::cout << "[Client] Arrêt du client...\n";
    m_network.stop();
}


void GameClient::sendInput(float& moveX, float& moveY, bool &shoot) {
    moveX = 0.f;
    moveY = 0.f;
    shoot = false;

    if (sf::Keyboard::isKeyPressed(sf::Keyboard::Left))
        moveX = -1.f;
    if (sf::Keyboard::isKeyPressed(sf::Keyboard::Right))
        moveX = 1.f;
    if (sf::Keyboard::isKeyPressed(sf::Keyboard::Up))
        moveY = -1.f;
    if (sf::Keyboard::isKeyPressed(sf::Keyboard::Down))
        moveY = 1.f;
    if (sf::Keyboard::isKeyPressed(sf::Keyboard::Space))
        shoot = true;

    std::cout << "[Client] Input simulé: move(" << moveX << "," << moveY 
              << ") shoot=" << shoot << std::endl;
}

void GameClient::handleSnapshot(const SnapshotPacket& snapshot) {
     worldEntities.clear();

    for (int i = 0; i < snapshot.num_entities; i++) {
        auto e = snapshot.entities[i];
        worldEntities[e.id] = {e.x, e.y};
    }
    std::cout << "[Client] Snapshot reçu: " << snapshot.num_entities << " entités" << std::endl;
}

void GameClient::render() {
    std::cout << "[Client] Rendu des entités:\n";
    for (auto& [id, ent] : worldEntities) {
        std::cout << " -> Entité " << id << " (" << ent.x << "," << ent.y << ")" << std::endl;
    }
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
    
    std::cout << "[Client] Menu lancé - Utilisez les flèches Haut/Bas pour naviguer, Entrée pour sélectionner\n";
    
    while (window.isOpen() && inMenu) {
        float deltaTime = clock.restart().asSeconds();
        
        sf::Event event;
        while (window.pollEvent(event)) {
            if (event.type == sf::Event::Closed) {
                window.close();
                inMenu = false;
                return;
            }
            
            if (event.type == sf::Event::KeyPressed) {
                if (event.key.code == sf::Keyboard::Up) {
                    menu->navigateUp();
                    std::cout << "[Menu] Navigation: Haut (bouton " << menu->selectedButtonIndex << ")\n";
                }
                else if (event.key.code == sf::Keyboard::Down) {
                    menu->navigateDown();
                    std::cout << "[Menu] Navigation: Bas (bouton " << menu->selectedButtonIndex << ")\n";
                }
                else if (event.key.code == sf::Keyboard::Return || event.key.code == sf::Keyboard::Space) {
                    MenuState newState = menu->selectCurrentButton();
                    std::cout << "[Menu] Sélection validée\n";
                    
                    if (newState == MenuState::NewGame) {
                        std::cout << "[Menu] Lancement d'une nouvelle partie...\n";
                        inMenu = false;
                        window.close();
                        return;
                    }
                    else if (newState == MenuState::Quit) {
                        std::cout << "[Menu] Fermeture du jeu...\n";
                        window.close();
                        inMenu = false;
                        return;
                    }
                    else if (newState == MenuState::Continue) {
                        std::cout << "[Menu] Continuer (non implémenté)\n";
                    }
                    else if (newState == MenuState::Options) {
                        std::cout << "[Menu] Options (non implémenté)\n";
                    }
                    else if (newState == MenuState::HighScore) {
                        std::cout << "[Menu] Meilleurs scores (non implémenté)\n";
                    }
                    else if (newState == MenuState::Credits) {
                        std::cout << "[Menu] Crédits (non implémenté)\n";
                    }
                }
                else if (event.key.code == sf::Keyboard::Escape) {
                    std::cout << "[Menu] Échap pressé - Fermeture\n";
                    window.close();
                    inMenu = false;
                    return;
                }
            }
            
            if (event.type == sf::Event::MouseButtonPressed) {
                if (event.mouseButton.button == sf::Mouse::Left) {
                    sf::Vector2f mousePos(static_cast<float>(event.mouseButton.x), 
                                         static_cast<float>(event.mouseButton.y));
                    MenuState newState = menu->handleClick(mousePos);
                    
                    if (newState == MenuState::NewGame) {
                        std::cout << "[Menu] Nouvelle partie (souris)\n";
                        inMenu = false;
                        window.close();
                        return;
                    }
                    else if (newState == MenuState::Quit) {
                        std::cout << "[Menu] Quitter (souris)\n";
                        window.close();
                        inMenu = false;
                        return;
                    }
                }
            }
        }
        
        sf::Vector2i mousePixelPos = sf::Mouse::getPosition(window);
        sf::Vector2f mousePos = window.mapPixelToCoords(mousePixelPos);
        menu->update(mousePos);
        
        logo.update(deltaTime);
        for (auto& star : stars) {
            star.update(deltaTime);
        }
        updateParticles(particles, particleClock, deltaTime);
        
        window.clear(sf::Color(10, 10, 30));
        
        drawGrid(window, gridOffsetX, gridOffsetY);
        for (const auto& star : stars) {
            star.draw(window);
        }
        for (const auto& particle : particles) {
            particle.draw(window);
        }
        
        logo.draw(window);
        menu->draw(window);
        
        window.display();
    }
}

void GameClient::run() {
    runMenu();

    if (!inMenu && menu && menu->state != MenuState::NewGame) {
        std::cout << "[Client] Retour au menu ou fermeture\n";
        return;
    }

    std::cout << "[Client] Connexion réseau et démarrage du jeu...\n";

    sf::RenderWindow window(sf::VideoMode(800, 600), "R-Type Client - Jeu");
    window.setFramerateLimit(60);

    while (window.isOpen()) {
        sf::Event event;
        while (window.pollEvent(event)) {
            if (event.type == sf::Event::Closed)
                window.close();
        }

        float moveX, moveY;
        bool shoot;
        sendInput(moveX, moveY, shoot);

        InputPacket input{};
        input.header.type = PacketType::INPUT;
        input.header.size = sizeof(InputPacket);
        input.player_id = playerId;
        input.move_x = moveX;
        input.move_y = moveY;
        input.shoot = shoot;

        m_network.sendInput(input);

        SnapshotPacket snap{};
        if (m_network.pollSnapshot(snap)) {
            handleSnapshot(snap);
        }

        window.clear();
        render();
        window.display();

        std::this_thread::sleep_for(std::chrono::milliseconds(1000 / 30));
    }

    shutdown();
}
