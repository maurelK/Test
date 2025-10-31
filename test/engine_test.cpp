#include "../include/Core/GameEngine.hpp"
#include <iostream>
#include <cassert>

// Composants de test
struct TestPosition { float x, y; };
struct TestVelocity { float dx, dy; };

void test_entity_creation() {
    std::cout << "🧪 Test création d'entités..." << std::endl;
    
    GameEngine engine;
    EngineConfig config;
    engine.initialize(GameEngine::Mode::STANDALONE, config);
    
    // Test création entité
    Entity e1 = engine.getECS().createEntity();
    assert(e1 != INVALID_ENTITY);
    
    // Test composants
    engine.getECS().registerComponent<TestPosition>();
    engine.getECS().addComponent<TestPosition>(e1, {100.0f, 200.0f});
    
    auto& pos = engine.getECS().getComponent<TestPosition>(e1);
    assert(pos.x == 100.0f && pos.y == 200.0f);
    
    std::cout << "✅ Test réussi !" << std::endl;
}

void test_system_registration() {
    std::cout << "🧪 Test systèmes..." << std::endl;
    
    class TestSystem : public System {
    public:
        void update(float dt) override {
            std::cout << "System update: " << dt << std::endl;
        }
    };
    
    GameEngine engine;
    EngineConfig config;
    engine.initialize(GameEngine::Mode::STANDALONE, config);
    
    auto system = engine.getECS().registerSystem<TestSystem>();
    assert(system != nullptr);
    
    // Test boucle de jeu
    engine.update(0.016f); // 60 FPS
    
    std::cout << "✅ Test réussi !" << std::endl;
}

int main() {
    std::cout << "🚀 TEST DU MOTEUR SEUL" << std::endl;
    
    test_entity_creation();
    test_system_registration();
    
    std::cout << "🎉 TOUS LES TESTS PASSÉS !" << std::endl;
    return 0;
}