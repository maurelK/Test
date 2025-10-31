*/*
** Test de compilation du engine seul
** Sans AUCUNE référence à R-Type
*/

#include "engine/include/Core/GameEngine.hpp"
#include <iostream>

// Composants de test GÉNÉRIQUES
struct TestComponent { 
    std::string name; 
    int value; 
};

// Système de test GÉNÉRIQUE  
class TestSystem : public System {
public:
    void update(float dt) override {
        std::cout << "TestSystem update: " << dt << std::endl;
    }
};

int main() {
    std::cout << "🧪 COMPILATION ENGINE SEUL" << std::endl;
    
    // ✅ Le engine compile sans R-Type
    GameEngine engine;
    EngineConfig config;
    
    // Configuration générique
    config.window.title = "Engine Test";
    config.window.width = 800;
    config.window.height = 600;
    
    // Initialisation STANDALONE (sans jeu)
    if (!engine.initialize(GameEngine::Mode::STANDALONE, config)) {
        std::cerr << "❌ Engine initialization failed!" << std::endl;
        return 1;
    }
    
    // Test ECS générique
    engine.getECS().registerComponent<TestComponent>();
    auto testSystem = engine.getECS().registerSystem<TestSystem>();
    
    Entity testEntity = engine.getECS().createEntity();
    engine.getECS().addComponent<TestComponent>(testEntity, {"Test", 42});
    
    std::cout << "✅ ENGINE COMPILE ET FONCTIONNE SEUL !" << std::endl;
    
    // Boucle de test
    engine.run();
    
    return 0;
}