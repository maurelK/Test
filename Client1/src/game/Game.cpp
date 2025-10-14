#include "Game.hpp"
#include <iostream>

Game::Game() 
    : window(sf::VideoMode(1280, 720), "R-TYPE - GAME"), 
      state(GameState::Playing),
      player(100.f, 360.f),
      score(0), lives(3), level(1), enemiesKilled(0) {
    
    window.setFramerateLimit(60);
    
    if (!font.loadFromFile("assets/logo_font.ttf")) {
        std::cout << "Police non chargee" << std::endl;
    }
    
    themes = {
        {sf::Color(10, 14, 39), sf::Color::White, "ESPACE PROFOND"},
        {sf::Color(26, 10, 46), sf::Color::Magenta, "NEBULEUSE POURPRE"},
        {sf::Color(15, 32, 39), sf::Color::Cyan, "ZONE GLACIALE"},
        {sf::Color(45, 27, 0), sf::Color(255, 165, 0), "CEINTURE D'ASTEROIDES"}
    };
}

void Game::run() {
    while (window.isOpen()) {
        handleEvents();
        float deltaTime = clock.restart().asSeconds();
        update(deltaTime);
        draw();
    }
}

void Game::handleEvents() {
    sf::Event event;
    while (window.pollEvent(event)) {
        if (event.type == sf::Event::Closed) {
            window.close();
        }
        
        if (event.type == sf::Event::KeyPressed) {
            if (event.key.code == sf::Keyboard::Escape) {
                window.close();
            }
            if (event.key.code == sf::Keyboard::Space && state == GameState::Playing) {
                bullets.push_back(player.shoot());
            }
        }
        
        if (event.type == sf::Event::MouseButtonPressed && state == GameState::GameOver) {
            sf::Vector2i mousePos = sf::Mouse::getPosition(window);
            if (mousePos.x >= 540 && mousePos.x <= 740 && mousePos.y >= 400 && mousePos.y <= 460) {
                score = 0;
                lives = 3;
                level = 1;
                enemiesKilled = 0;
                enemies.clear();
                bullets.clear();
                enemyBullets.clear();
                powerUps.clear();
                particles.clear();
                state = GameState::Playing;
            }
        }
    }
}

void Game::update(float deltaTime) {
    if (state == GameState::LevelTransition) {
        if (transitionClock.getElapsedTime().asSeconds() > 3.f) {
            state = GameState::Playing;
        }
        return;
    }
    
    if (state != GameState::Playing) return;
    
    player.update(deltaTime);
    
    for (auto it = bullets.begin(); it != bullets.end();) {
        it->update(deltaTime);
        if (it->isOutOfBounds(1280)) {
            it = bullets.erase(it);
        } else {
            ++it;
        }
    }
    
    for (auto it = enemyBullets.begin(); it != enemyBullets.end();) {
        it->update(deltaTime);
        if (it->getBounds().intersects(player.getBounds())) {
            lives--;
            createParticles(player.getPosition().x, player.getPosition().y, sf::Color::Red, 15);
            it = enemyBullets.erase(it);
            if (lives <= 0) gameOver();
        } else if (it->isOutOfBounds(1280)) {
            it = enemyBullets.erase(it);
        } else {
            ++it;
        }
    }
    
    if (enemySpawnClock.getElapsedTime().asSeconds() > std::max(1.5f - level * 0.1f, 0.5f)) {
        spawnEnemy();
        enemySpawnClock.restart();
    }
    
    for (auto& enemy : enemies) {
        enemy.update(deltaTime);
        if (enemy.canShoot() && rand() % 100 < 2) {
            sf::Vector2f pos = enemy.getPosition();
            enemyBullets.emplace_back(pos.x, pos.y + 20, false);
        }
    }
    
    enemies.erase(std::remove_if(enemies.begin(), enemies.end(),
        [](const Enemy& e) { return e.getPosition().x < -100.f; }), enemies.end());
    
    for (auto it = powerUps.begin(); it != powerUps.end();) {
        it->update(deltaTime);
        if (it->getBounds().intersects(player.getBounds())) {
            if (it->getType() == PowerUpType::Life) {
                lives = std::min(lives + 1, 5);
            } else {
                score += 500;
            }
            createParticles(it->getBounds().left, it->getBounds().top, 
                           it->getType() == PowerUpType::Life ? sf::Color::Green : sf::Color::Yellow, 20);
            it = powerUps.erase(it);
        } else if (it->isOutOfBounds()) {
            it = powerUps.erase(it);
        } else {
            ++it;
        }
    }
    
    for (auto& p : particles) {
        p.update(deltaTime);
    }
    particles.erase(std::remove_if(particles.begin(), particles.end(),
        [](const GameParticle& p) { return p.life <= 0; }), particles.end());
    
    checkCollisions();
    
    if (enemiesKilled >= 2) {
        levelUp();
    }
}

void Game::draw() {
    LevelTheme theme = themes[std::min(level - 1, (int)themes.size() - 1)];
    window.clear(theme.bg);
    
    for (int i = 0; i < 150; i++) {
        sf::CircleShape star(rand() % 3);
        star.setPosition(rand() % 1280, rand() % 720);
        star.setFillColor(theme.stars);
        window.draw(star);
    }
    
    for (auto& p : particles) {
        p.draw(window);
    }
    
    player.draw(window);
    
    for (auto& bullet : bullets) {
        bullet.draw(window);
    }
    
    for (auto& bullet : enemyBullets) {
        bullet.draw(window);
    }
    
    for (auto& enemy : enemies) {
        enemy.draw(window);
    }
    
    for (auto& powerUp : powerUps) {
        powerUp.draw(window);
    }
    
    sf::Text livesText("VIES: " + std::to_string(lives), font, 20);
    livesText.setPosition(20, 20);
    livesText.setFillColor(sf::Color::Cyan);
    window.draw(livesText);
    
    sf::Text scoreText("SCORE: " + std::to_string(score), font, 20);
    scoreText.setPosition(20, 50);
    scoreText.setFillColor(sf::Color::Cyan);
    window.draw(scoreText);
    
    sf::Text levelText("NIVEAU: " + std::to_string(level), font, 20);
    levelText.setPosition(20, 80);
    levelText.setFillColor(sf::Color::Cyan);
    window.draw(levelText);
    
    sf::Text enemiesText("ENNEMIS: " + std::to_string(enemies.size()), font, 20);
    enemiesText.setPosition(20, 110);
    enemiesText.setFillColor(sf::Color::Cyan);
    window.draw(enemiesText);
    
    if (state == GameState::LevelTransition) {
        sf::Text transitionText(theme.name, font, 64);
        sf::FloatRect bounds = transitionText.getLocalBounds();
        transitionText.setOrigin(bounds.width / 2, bounds.height / 2);
        transitionText.setPosition(640, 360);
        transitionText.setFillColor(sf::Color::Yellow);
        window.draw(transitionText);
    }
    
    if (state == GameState::GameOver) {
        sf::Text gameOverText("GAME OVER", font, 64);
        sf::FloatRect bounds = gameOverText.getLocalBounds();
        gameOverText.setOrigin(bounds.width / 2, bounds.height / 2);
        gameOverText.setPosition(640, 250);
        gameOverText.setFillColor(sf::Color(255, 107, 53));
        window.draw(gameOverText);
        
        sf::Text finalScore("Score Final: " + std::to_string(score), font, 32);
        bounds = finalScore.getLocalBounds();
        finalScore.setOrigin(bounds.width / 2, bounds.height / 2);
        finalScore.setPosition(640, 340);
        finalScore.setFillColor(sf::Color::White);
        window.draw(finalScore);
        
        sf::RectangleShape replayBtn(sf::Vector2f(200, 60));
        replayBtn.setPosition(540, 400);
        replayBtn.setFillColor(sf::Color::Transparent);
        replayBtn.setOutlineColor(sf::Color::Cyan);
        replayBtn.setOutlineThickness(3);
        window.draw(replayBtn);
        
        sf::Text replayText("REJOUER", font, 24);
        bounds = replayText.getLocalBounds();
        replayText.setOrigin(bounds.width / 2, bounds.height / 2);
        replayText.setPosition(640, 430);
        replayText.setFillColor(sf::Color::Cyan);
        window.draw(replayText);
    }
    
    sf::Text instructions("Deplacer | ESPACE Tirer | ESC Quitter", font, 16);
    sf::FloatRect bounds = instructions.getLocalBounds();
    instructions.setOrigin(bounds.width / 2, 0);
    instructions.setPosition(640, 690);
    instructions.setFillColor(sf::Color(255, 255, 255, 100));
    window.draw(instructions);
    
    window.display();
}

void Game::spawnEnemy() {
    float y = 50.f + static_cast<float>(rand() % 600);
    enemies.emplace_back(1200.f, y, level);
}

void Game::checkCollisions() {
    for (auto bulletIt = bullets.begin(); bulletIt != bullets.end();) {
        bool removed = false;
        for (auto enemyIt = enemies.begin(); enemyIt != enemies.end();) {
            if (bulletIt->getBounds().intersects(enemyIt->getBounds())) {
                enemyIt->takeDamage(1);
                createParticles(enemyIt->getPosition().x, enemyIt->getPosition().y, 
                               sf::Color::Red, 5);
                
                if (enemyIt->isDead()) {
                    score += enemyIt->getPoints() * level;
                    enemiesKilled++;
                    createParticles(enemyIt->getPosition().x, enemyIt->getPosition().y, 
                                   sf::Color::Yellow, 20);
                    if (rand() % 100 < 30) {
                        powerUps.emplace_back(enemyIt->getPosition().x, enemyIt->getPosition().y,
                                             rand() % 2 == 0 ? PowerUpType::Life : PowerUpType::Score);
                    }
                    enemyIt = enemies.erase(enemyIt);
                } else {
                    ++enemyIt;
                }
                
                bulletIt = bullets.erase(bulletIt);
                removed = true;
                break;
            } else {
                ++enemyIt;
            }
        }
        if (!removed) ++bulletIt;
    }
    for (auto& enemy : enemies) {
        if (player.getBounds().intersects(enemy.getBounds())) {
            lives--;
            createParticles(player.getPosition().x, player.getPosition().y, sf::Color::Red, 20);
            if (lives <= 0) gameOver();
            break;
        }
    }
}

void Game::levelUp() {
    level++;
    enemiesKilled = 0;
    state = GameState::LevelTransition;
    transitionClock.restart();
}

void Game::gameOver() {
    state = GameState::GameOver;
}

void Game::createParticles(float x, float y, sf::Color color, int count) {
    for (int i = 0; i < count; i++) {
        particles.emplace_back(x, y, color);
    }
}