/*
** EPITECH PROJECT, 2025
** main
** File description:
** server
*/

#include <iostream>
#include <chrono>
#include <thread>
#include "system/GameServer.hpp"
#include "Network/Networkmanager.hpp"

int main()
{
    try {
        std::cout << "[SERVER] Starting game server..." << std::endl;

        // Initialiser le réseau
        NetworkManager& netManager = NetworkManager::getInstance();
        if (!netManager.initialize(9090, 9091)) {
            std::cerr << "[ERROR] Failed to initialize network" << std::endl;
            return 1;
        }
        std::cout << "[SERVER] Network initialized (TCP: 9090, UDP: 9091)" << std::endl;

        // Initialiser le serveur de jeu
        GameServer server;
        server.initialize();
        std::cout << "[SERVER] Game server initialized. Running game loop..." << std::endl;

        // Boucle principale du serveur
        const float dt = 0.016f; // ~60 FPS
        while (true) {
            // Traiter les inputs réseau
            InputPacket input;
            while (netManager.hasInputs()) {
                if (netManager.popInput(input)) {
                    // TODO: Traiter les inputs des joueurs
                    std::cout << "[INPUT] Received input from player " << input.player_id << std::endl;
                }
            }

            // Mettre à jour la logique du jeu
            server.update(dt);

            // Envoyer les snapshots aux clients
            // TODO: Créer et envoyer les snapshots via netManager.sendSnapshot()

            std::this_thread::sleep_for(std::chrono::milliseconds(16));
        }

        netManager.shutdown();
        std::cout << "[SERVER] Server stopped cleanly." << std::endl;
        return 0;
    } catch (const std::exception& e) {
        std::cerr << "[ERROR] Exception: " << e.what() << std::endl;
        return 1;
    }
}
