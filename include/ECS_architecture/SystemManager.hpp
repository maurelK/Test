/*
** EPITECH PROJECT, 2025
** SystemManager
** File description:
** Handle OUr System
*/


#ifndef SYSTEM_MANAGER_HPP
#define SYSTEM_MANAGER_HPP

#include "base.hpp"
#include "System.hpp"
#include <memory>
#include <unordered_map>
#include <vector>
#include <typeindex>
#include "Component_Manager.hpp"

class SceneManager {
    std::vector<std::unique_ptr<Scene>> sceneStack;
    
public:
    void pushScene(std::unique_ptr<Scene> scene) {
        if (!sceneStack.empty()) {
            sceneStack.back()->onExit();
        }
        sceneStack.push_back(std::move(scene));
        sceneStack.back()->onEnter();
    }

    void popScene() {
        if (!sceneStack.empty()) {
            sceneStack.back()->onExit();
            sceneStack.pop_back();
        }
        if (!sceneStack.empty()) {
            sceneStack.back()->onEnter();
        }
    }

    void changeScene(std::unique_ptr<Scene> scene) {
        while (!sceneStack.empty()) {
            popScene();
        }
        pushScene(std::move(scene));
    }

    void update(float deltaTime) {
        if (!sceneStack.empty()) {
            sceneStack.back()->update(deltaTime);
        }
    }

    void render() {
        if (!sceneStack.empty()) {
            sceneStack.back()->render();
        }
    }
};
#endif