/*
** EPITECH PROJECT, 2025
** Handlee Scene
** File description:
** Every Scene that we will have has Menu Scene that containt option ..."
*/

#ifndef SCENE_MANAGER_HPP
#define SCENE_MANAGER_HPP

#include <memory>
#include <vector>
#include <iostream>  // ⭐ AJOUTER pour std::cout

class Scene {
public:
    virtual ~Scene() = default;
    virtual void onEnter() = 0;
    virtual void onExit() = 0;
    virtual void update(float deltaTime) = 0;
    virtual void render() = 0;
};

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
