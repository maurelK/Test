/*
** Test de compilation du engine seul
*/

#include "../include/Core/GameEngine.hpp"
#include "TestScene.hpp"  // ⭐ AJOUTER
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
        static int callCount = 0;
        callCount++;
        if (callCount % 60 == 0) {
            std::cout << "⚙️ TestSystem update #" << callCount 
                      << " (dt: " << dt << ")" << std::endl;
        }
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
    
    // Créer des entités de test
    Entity testEntity = engine.getECS().createEntity();
    engine.getECS().addComponent<TestComponent>(testEntity, {"TestEntity", 42});
    
    Entity testEntity2 = engine.getECS().createEntity();
    engine.getECS().addComponent<TestComponent>(testEntity2, {"TestEntity2", 1337});
    
    // Ajouter une scène de test
    engine.getScenes().pushScene(std::make_unique<TestScene>());
    
    std::cout << "✅ ENGINE COMPILÉ - LANCEMENT DU TEST..." << std::endl;
    std::cout << "Entities créées: " << testEntity << ", " << testEntity2 << std::endl;
    
    // Boucle de test
    engine.run();
    
    std::cout << "🎉 TEST TERMINÉ AVEC SUCCÈS !" << std::endl;
    return 0;
}