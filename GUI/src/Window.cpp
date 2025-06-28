#include "Window.hpp"

Window::Window()
{
    mode.width = 1920;
    mode.height = 1080;
    mode.bitsPerPixel = 32;
}

void Window::create()
{
    window.create(mode, "Zappy", sf::Style::Resize | sf::Style::Close);
}

void Window::destroy()
{
    window.close();
}

void Window::clear()
{
    window.clear(sf::Color::Black);
}

void Window::display()
{
    window.display();
}

bool Window::isOpen() const
{
    return window.isOpen();
}

void Window::pollEvent(sf::Event &event)
{
    window.pollEvent(event);
}
