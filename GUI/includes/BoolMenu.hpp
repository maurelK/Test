#ifndef BOOLMENU_HPP
    #define BOOLMENU_HPP
    #include <SFML/Graphics.hpp>

class BoolMenu
{
public:
    BoolMenu();
    bool showSprite3;
    bool showSprite4;
    bool Tf, Tf2, Tf3;
    sf::FloatRect rect, rect2, rect3;
};

#endif
