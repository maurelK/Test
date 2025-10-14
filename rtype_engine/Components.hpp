/*
** EPITECH PROJECT, 2025
** ef
** File description:
** fe
*/

#ifndef COMPONENTS_HPP
#define COMPONENTS_HPP

#include "base.hpp"
#include <SFML/Graphics.hpp>

struct Position {
    float x = 0.f;
    float y = 0.f;
    bool operator!=(const Position& o) const { return x != o.x || y != o.y; }
};

struct Velocity {
    float dx = 0.f;
    float dy = 0.f;
    bool operator!=(const Velocity& o) const { return dx != o.dx || dy != o.dy; }
};

struct Health {
    int hp = 0;
    bool operator!=(const Health& o) const { return hp != o.hp; }
};

// ajt bool frm_Player
struct Projectile {
    bool fromPlayer;
    Entity owner = static_cast<Entity>(-1);
    int damage = 1;
    float life = 5.0f;
};

// ------ Client-------
struct damage {
    int value;
};

struct enemy {
    int type;
};

struct Power_Up{
    int type;
};

struct Sprite{
    sf::Sprite sprite;
};

struct Player {
    int id;
};

#endif