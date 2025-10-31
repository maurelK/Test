// tests/TestScene.hpp
#ifndef TEST_SCENE_HPP
#define TEST_SCENE_HPP

#include "../include/Core/SceneManager.hpp"
#include <iostream>

class TestScene : public Scene {
private:
    int updateCount = 0;
    int renderCount = 0;

public:
    void onEnter() override {
        std::cout << "🔵 TestScene entered!" << std::endl;
    }

    void onExit() override {
        std::cout << "🔴 TestScene exited!" << std::endl;
    }

    void update(float deltaTime) override {
        updateCount++;
        if (updateCount % 60 == 0) {
            std::cout << "🔄 TestScene update #" << updateCount 
                      << " (delta: " << deltaTime << ")" << std::endl;
        }
    }

    void render() override {
        renderCount++;
        if (renderCount % 60 == 0) {
            std::cout << "🎨 TestScene render #" << renderCount << std::endl;
        }
    }
};

#endif