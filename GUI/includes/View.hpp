#ifndef VIEW_HPP
    #define VIEW_HPP
    #include <SFML/Graphics.hpp>

class View {
public:
    View(sf::RenderWindow &window);
    void initialize();
    void update();
    void handleEvent(const sf::Event &event);
    void draw(sf::RenderWindow &window);

private:
    sf::View view;
    sf::RenderWindow& window;
    float zoomLevel;
    void adjustView();
};

#endif