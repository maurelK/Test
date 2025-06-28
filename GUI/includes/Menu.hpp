#ifndef MENU_HPP
    #define MENU_HPP
    #include <SFML/Graphics.hpp>
    #include <vector>
    #include <string>

class Menu
{
public:
    Menu();
    void draw(sf::RenderWindow &window);
    void handleEvent(const sf::Event &event, bool &shouldStartGame);
    void moveUp();
    void moveDown();

private:
    std::vector<sf::Text> options;
    int selectedOptionIndex;
    sf::Font font;
    void setupTextOptions();
};

#endif
