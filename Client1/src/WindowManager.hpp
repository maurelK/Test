#ifndef WINDOWMANAGER_HPP
#define WINDOWMANAGER_HPP

#include <SFML/Graphics.hpp>
#include <string>

class WindowManager {
private:
    sf::RenderWindow* window;
    unsigned int width;
    unsigned int height;
    std::string title;

public:
    WindowManager(unsigned int w, unsigned int h, const std::string& t);
    ~WindowManager();
    
    bool createWindow();
    void closeWindow();
    bool isOpen() const;
    
    void clear(const sf::Color& color = sf::Color::Black);
    void display();
    
    bool pollEvent(sf::Event& event);
    sf::RenderWindow* getWindow() { return window; }
    
    unsigned int getWidth() const { return width; }
    unsigned int getHeight() const { return height; }
};

#endif // WINDOWMANAGER_HPP
