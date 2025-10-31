/*
** Test de compilation du engine seul
*/

#include "../include/Core/GameEngine.hpp"  // Chemin corrigé
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
    
    GameEngine engine;
    EngineConfig config;
    
    // Configuration générique
    config.window.title = "Engine Test";
    config.window.width = 800;
    config.window.height = 600;
    config.window.fpsLimit = 60;
    
    // Initialisation STANDALONE (sans jeu)
    if (!engine.initialize(GameEngine::Mode::STANDALONE, config)) {
        std::cerr << "❌ Engine initialization failed!" << std::endl;
        return 1;
    }
    
    // Test ECS générique
    engine.getECS().registerComponent<TestComponent>();
    auto testSystem = engine.getECS().registerSystem<TestSystem>();
    
    // Signature du système
    Signature testSig;
    testSig.set(engine.getECS().getComponentType<TestComponent>());
    engine.getECS().setSystemSignature<TestSystem>(testSig);
    
    Entity testEntity = engine.getECS().createEntity();
    engine.getECS().addComponent<TestComponent>(testEntity, {"Test", 42});
    
    std::cout << "✅ ENGINE COMPILE ET FONCTIONNE SEUL !" << std::endl;
    
    // Boucle de test (courte)
    engine.run();
    
    return 0;
}