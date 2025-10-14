#include "GameClient.hpp"
#include <iostream>

int main() {
    std::cout << "=== R-TYPE CLIENT ===" << std::endl;
    std::cout << "[INFO] Initialisation du client..." << std::endl;

    try {
        // Nom du joueur (temporaire, peut venir d’un écran d’entrée plus tard)
        std::string username = "Player1";

        // Création du client
        GameClient client(username);

        // Lancement du menu principal (mode selection)
        client.runClient();

        std::cout << "[INFO] Fermeture du client proprement." << std::endl;
    }
    catch (const std::exception& e) {
        std::cerr << "[ERREUR FATALE] Exception : " << e.what() << std::endl;
        return EXIT_FAILURE;
    }
    catch (...) {
        std::cerr << "[ERREUR FATALE] Exception inconnue." << std::endl;
        return EXIT_FAILURE;
    }

    return EXIT_SUCCESS;
}
