/*
** EPITECH PROJECT, 2025
** fef
** File description:
** effe
*/

#include <iostream>
#include "GameServer.hpp"

int main() {
    GameServer server;

    server.spawnEnemy();
    server.spawnEnemy();

    Snapshot snap = server.getSnapshot();

    for (const auto& e : snap.entities) {
        std::cout << "Entity " << e.id
                  << " | Position(" << e.pos.x << "," << e.pos.y << ")"
                  << " | Velocity(" << e.vel.dx << "," << e.vel.dy << ")"
                  << " | HP: " << e.hp.hp << "\n";
    }

    return 0;
}
