#include "GameClient.hpp"
#include <iostream>
#include <thread>
#include <chrono>

int main() {
    std::cout << "[Client] Lancement du jeu..." << std::endl;

    GameClient client("Pavel");
    if (!client.init())
        return 1;

    std::this_thread::sleep_for(std::chrono::seconds(2));

    client.run();

    std::cout << "[Client] Fin du prgmme" << std::endl;
    return 0;
}
