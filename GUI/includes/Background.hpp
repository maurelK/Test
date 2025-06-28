#ifndef BACKGROUND_HPP
    #define BACKGROUND_HPP
    #include <SFML/Graphics.hpp>
    #include <vector>
class Background {
public:
    Background();
    ~Background();
    void initialize();
    void update(float deltaTime);
    void draw(sf::RenderWindow &window);

private:
    std::vector<sf::Sprite> frames;
    sf::Clock clock;
    size_t currentFrame;
};

#endif