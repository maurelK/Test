/*
** EPITECH PROJECT, 2025
** feef
** File description:
** effe
*/

#pragma once
#include <bits/stdc++.h>

struct Color {
    float r, g, b;
    Color() : r(0), g(0), b(0) {}

    Color operator*(const float n) const {
        return Color(r * n, g * n, b * n);
    }

    Color clamp() const {
        return {
            std::min(1.0f, std::max(0.0f, r)),
            std::min(1.0f, std::max(0.0f, g)),
            std::min(1.0f, std::max(0.0f, b))
        };
    }

    Color& operator+=(const Color &color) {
        r += color.r;
        g += color.g;
        b += color.b;
        return *this;
    }

    Color operator+(const Color& color) {
        return {
            r + color.r,
            g + color.g,
            b + color.b
        };
    }

    Color(float r_, float g_, float b_) : r(r_), g(g_), b(b_) {}
};

