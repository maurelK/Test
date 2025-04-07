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
    #include <vector>
class IGraphical {
public:
    virtual ~IGraphical() = default;
    
    virtual void init() = 0;
    virtual void draw() = 0;
    virtual void refresh() = 0;
    virtual void close() = 0;
    virtual int getInput() = 0;
    virtual std::string getPlayerName() = 0;
    virtual std::string displayMenu(const std::vector<std::string> &games) = 0;
};
//extern "C" IGraphical* createInstance() {
//}
#endif
