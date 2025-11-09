/*
** EPITECH PROJECT, 2025
** ffrv
** File description:
** rgg
*/

#pragma once
// Components.hpp - définitions simples des composants
// Assure-toi que base.hpp définit `using Entity = unsigned int;` ou un typedef équivalent.

#include "base.hpp" // pour Entity typedef si présent
#include <cstdint>

struct Position {
    float x{0.f}, y{0.f};
};

struct Velocity {
    float dx{0.f}, dy{0.f};
};

struct Health {
    int hp{0};
};

// Projectile component : qui possède le projectile, ses dégâts et sa durée de vie
struct Projectile {
    Entity owner{static_cast<Entity>(-1)};
    int damage{0};
    float life{0.f};
};
