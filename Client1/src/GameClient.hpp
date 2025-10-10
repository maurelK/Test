#pragma once

#include <cstring>
#include <map>
#include <cstdint>
#include <string>
#include <unordered_map>
#include "Menu.hpp"
#include "Background.hpp"
#include "Logo.hpp"
#include "NetworkClient.hpp"

struct EntityVisual {
    float x;
    float y;
};

class GameClient {
public:
    GameClient(const std::string& username);
    ~GameClient();
    bool init();
    void shutdown();

    void sendInput(float& moveX, float& moveY, bool& shoot);
    void handleSnapshot(const struct SnapshotPacket& snapshot);
    void render();
    void run();
    void runMenu();

    const std::map<uint32_t, EntityVisual>& getEntities() const { return worldEntities; }
private:
    std::string username;
    uint32_t playerId;
    std::map<uint32_t, EntityVisual> worldEntities;


    Menu* menu;
    bool inMenu;

    NetworkClient m_network;

};
