#include "GameClient.hpp"
#include <iostream>

int main() {
    std::cout << "=== R-TYPE CLIENT ===" << std::endl;
    std::cout << "[INFO] Initialisation du client..." << std::endl;

    try {
        std::string username = "Player1";

        GameClient client(username);

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
