#ifndef WINDOW_HPP
    #define WINDOW_HPP
    #include <SFML/Graphics.hpp>

class Window {
public:
    Window();
    void create();
    void destroy();
    void clear();
    void display();
    bool isOpen() const;
    void pollEvent(sf::Event &event);

    sf::RenderWindow window;
    sf::Event event;
    sf::VideoMode mode;
};

#endif
