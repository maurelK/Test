#ifndef LOADING_HPP
    #define LOADING_HPP
    #include <SFML/Graphics.hpp>

class Loading {
public:
    Loading();
    void initialize();
    void update(float progress);
    sf::Sprite &getSprite();

private:
    sf::Sprite sprite;
    sf::Texture texture;
    sf::Vector2u size;
    float progress;
};

#endif