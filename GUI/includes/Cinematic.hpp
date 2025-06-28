#ifndef CINEMATIC_HPP
    #define CINEMATIC_HPP
    #include "Window.hpp"
    #include "Loading.hpp"
    #include <SFML/Graphics.hpp>

class Cinematic
{
public:
    Cinematic();
    void initialize();
    void animate(Window &window, Loading &loading);
    void centerSprite(sf::Sprite &sprite);
    void reset();
    void destroy();

private:
    sf::Sprite image, image2, image3;
    sf::Texture iTexture, iTexture2, iTexture3;
    bool isCinematicPlay;
};

#endif