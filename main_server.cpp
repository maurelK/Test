/*
** EPITECH PROJECT, 2025
** main
** File description:
** server
*/

#include "system/GameServer.hpp"
#include "Network/Networkmanager.hpp"
#include "Network/protocol.hpp"
#include <iostream>
#include <thread>
#include <chrono>
#include "Network/Networkmanager.hpp"
#include <iostream>
#include <thread>
#include <chrono>

int main() {
    std::cout << "=== Lancement du serveur R-Type ===\n";

    auto& network = NetworkManager::getInstance();
    if (!network.initialize(9090, 9091)) {
        std::cerr << "Impossible de démarrer le réseau !" << std::endl;
        return 1;
    }

    GameServer server;
    server.initialize();

    std::cout << "Serveur démarré. En attente des joueurs...\n";
    const float dt = 1.0f / 60.0f;
    while (true) {
        server.update(dt);
        std::this_thread::sleep_for(std::chrono::duration<float>(dt));
    }
    network.shutdown();
    return 0;
}
