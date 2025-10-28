/*
** EPITECH PROJECT, 2025
** R-TYPE
** File description:
** Param to our games"
*/

#include "../ECS_architecture/Orchestror.hpp"

class GameState {
private:
    Orchestror orchestrator;
    
public:
    void init();
    void update(float dt);
    void render();
    void cleanup();
};

