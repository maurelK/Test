#include "NetworkClient.hpp"
#include <thread>
#include <chrono>
/*
int main() {
    NetworkClient client("127.0.0.1", 9091);
    if (!client.start()) return 1;

    client.sendLogin("Pavel");

    for (int i = 0; i < 10; ++i) {
        bool success = false;
        if (client.pollLoginAck(success)) {
            std::cout << " Login ACK reçu !\n";
        }

        SnapshotPacket snap;
        if (client.pollSnapshot(snap)) {
            std::cout << " Snapshot reçu - entités: " << snap.num_entities << "\n";
        }

        std::this_thread::sleep_for(std::chrono::milliseconds(500));
    }

    client.stop();
    return 0;
}
*/