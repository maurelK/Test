#pragma once
#include "base.hpp"
#include <SFML/Graphics.hpp>

struct Position {
    float x;
    float y;
};

struct Velocity {
    float vx;
    float vy;
};

struct Projectile {
    bool fromPlayer;
};

struct Sprite {
    sf::Sprite sprite;
};

struct Player {
    int id;
};


// santé (point de vie)
struct Health {
    int hp;
};

// Les bonus que le plyr peut take
struct  PowerUp{
    int type;
};

struct enemy {
    int type;
};

struct damage {
    int value;
};
