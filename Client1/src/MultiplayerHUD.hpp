#ifndef MULTIPLAYER_HUD_HPP
#define MULTIPLAYER_HUD_HPP

#include <SFML/Graphics.hpp>
#include <string>
#include <vector>
#include "../../Network/protocol.hpp"

class MultiplayerHUD {
private:
    sf::Font font;
    bool fontLoaded;
    
    // Panneaux pour chaque joueur (style simple comme le jeu solo)
    struct PlayerPanel {
        sf::Text nameText;
        sf::Text livesText;
        sf::Text scoreText;
        sf::Text killsText;
    };
    
    std::vector<PlayerPanel> playerPanels;
    uint32_t localPlayerId;
    
public:
    MultiplayerHUD();
    
    void setLocalPlayerId(uint32_t id) { localPlayerId = id; }
    
    void updateStats(const std::vector<PlayerStats>& stats);
    
    void draw(sf::RenderTarget& target);
    
    bool isReady() const { return fontLoaded; }
};

#endif // MULTIPLAYER_HUD_HPP
