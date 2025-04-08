/*
** EPITECH PROJECT, 2025
** Class Igraphical
** File description:
** IGraphical of my program
*/

#ifndef IGRAPHICAL_HPP
    #define IGRAPHICAL_HPP
    #include <string>
    #include <iostream>
    #include <string>
    #include <vector>
    #include "IGame.hpp"

class IGraphical
{
public:
    virtual ~IGraphical() = default;
    virtual void init() = 0;
    virtual void close() = 0;
    virtual void render(const RenderData &data) = 0;
    virtual int getInput() = 0;
};

extern "C"
{
    IGraphical *createGraphical();
    void deleteGraphical(IGraphical *graphical);
}

#endif