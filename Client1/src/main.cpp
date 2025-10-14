#include "GameClient.hpp"
#include <iostream>
#include <thread>
#include <chrono>

int main() {
    std::cout << "[CLIENT] Lancement du client R-Type..." << std::endl;

    bool connected = false;
    int attempts = 0;

    while (!connected && attempts < 10) {
        std::cout << "[CLIENT] Tentative de connexion au serveur... (" << attempts + 1 << ")\n";
        GameClient client("Pavel");

        if (client.init()) {
            connected = true;
            std::cout << "[CLIENT] Connecté au serveur avec succès !\n";
            client.run();
        } else {
            std::cerr << "[CLIENT] Serveur indisponible, nouvelle tentative dans 2s...\n";
            std::this_thread::sleep_for(std::chrono::seconds(2));
            attempts++;
        }
    }

    if (!connected) {
        std::cerr << "[CLIENT] Impfsible de se connecter au serveur après 10 tentatives.\n";
        return 1;
    }

    return 0;
}
