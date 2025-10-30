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
    void pushScene(std::unique_ptr<Scene> scene);
    void popScene();
    void changeScene(std::unique_ptr<Scene> scene);
    
    void update(float deltaTime);
    void render();
};

#endif