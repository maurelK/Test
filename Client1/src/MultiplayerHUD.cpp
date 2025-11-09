#include "MultiplayerHUD.hpp"
#include <iostream>

MultiplayerHUD::MultiplayerHUD() : fontLoaded(false), localPlayerId(0) {
    if (!font.loadFromFile("assets/logo_font.ttf")) {
        std::cerr << "✗ Impossible de charger la police pour le HUD multijoueur\n";
        fontLoaded = false;
    } else {
        std::cout << "✓ Police HUD multijoueur chargée\n";
        fontLoaded = true;
    }
}

void MultiplayerHUD::updateStats(const std::vector<PlayerStats>& stats) {
    if (!fontLoaded) return;
    
    playerPanels.clear();
    
    for (size_t i = 0; i < stats.size(); i++) {
        const auto& stat = stats[i];
        PlayerPanel panel;
        
        // Position : joueur local à gauche, autres à droite
        float posX;
        float startY = 20.f;
        
        if (stat.player_id == localPlayerId) {
            posX = 20.f; // Gauche (comme dans le jeu solo)
        } else {
            posX = 1100.f; // Droite
        }
        
        // Titre du joueur
        panel.nameText.setFont(font);
        panel.nameText.setCharacterSize(20);
        panel.nameText.setFillColor(sf::Color::Yellow);
        panel.nameText.setStyle(sf::Text::Bold);
        
        if (stat.player_id == localPlayerId) {
            panel.nameText.setString("=== VOUS ===");
        } else {
            panel.nameText.setString("=== JOUEUR " + std::to_string(stat.player_id) + " ===");
        }
        
        panel.nameText.setPosition(posX, startY);
        
        // Vies
        panel.livesText.setFont(font);
        panel.livesText.setCharacterSize(20);
        panel.livesText.setFillColor(sf::Color::Cyan);
        panel.livesText.setString("VIES: " + std::to_string(stat.lives));
        panel.livesText.setPosition(posX, startY + 30.f);
        
        // Score
        panel.scoreText.setFont(font);
        panel.scoreText.setCharacterSize(20);
        panel.scoreText.setFillColor(sf::Color::Cyan);
        panel.scoreText.setString("SCORE: " + std::to_string(stat.score));
        panel.scoreText.setPosition(posX, startY + 60.f);
        
        // Ennemis tués
        panel.killsText.setFont(font);
        panel.killsText.setCharacterSize(20);
        panel.killsText.setFillColor(sf::Color::Cyan);
        panel.killsText.setString("KILLS: " + std::to_string(stat.enemies_killed));
        panel.killsText.setPosition(posX, startY + 90.f);
        
        // Pas besoin de background ni healthBar pour rester simple comme le solo
        
        playerPanels.push_back(panel);
    }
}

void MultiplayerHUD::draw(sf::RenderTarget& target) {
    if (!fontLoaded) return;
    
    for (const auto& panel : playerPanels) {
        target.draw(panel.nameText);
        target.draw(panel.livesText);
        target.draw(panel.scoreText);
        target.draw(panel.killsText);
    }
}
