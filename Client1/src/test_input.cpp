#include "Orchestror.hpp"
#include "MovementSystem.hpp"
#include "InputSystem.hpp"
#include <SFML/Graphics.hpp>
#include <iostream>
#include "../rtype_engine/Components.hpp"
#include "../Network/protocol.hpp"
#include "NetworkClient.hpp"
#include <iostream>
#include <thread>

int main() {
    NetworkClient client("127.0.0.1", 9091);

    if (!client.start()) {
        std::cerr << "[Client] Échec connexion serveur.\n";
        return 1;
    }

    std::cout << "[Client] Envoi d’inputs automatiques...\n";

    for (int i = 0; i < 10; ++i) {
        InputPacket input{};
        input.header.type = PacketType::INPUT;
        input.header.size = sizeof(InputPacket);
        input.player_id = 1;
        input.move_x = (i % 2 == 0) ? 1.f : -1.f;
        input.move_y = 0.f;
        input.shoot = (i % 3 == 0);

        client.sendInput(input);
        std::this_thread::sleep_for(std::chrono::milliseconds(500));
    }

    std::cout << "[Client] Fin du test.\n";
    return 0;
}
