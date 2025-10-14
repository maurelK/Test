#include "GameClient.hpp"
#include <iostream>

int main() {
    std::cout << "[TEST] Lancement du client R-Type en mode solo (offline)...\n";

    GameClient client("Pavel");
    if (!client.initLocal()) {
        std::cerr << "[TEST] Erreur: initialisation client échouée.\n";
        return 1;
    }

    client.runLocalGame(); // ✅ version solo complète (fond, tirs, ennemis, etc.)
    return 0;
}
