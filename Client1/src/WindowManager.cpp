#include "WindowManager.hpp"

WindowManager::WindowManager(unsigned int w, unsigned int h, const std::string& t)
    : window(nullptr), width(w), height(h), title(t) {}

WindowManager::~WindowManager() {
    if (window) {
        delete window;
        window = nullptr;
    }
}

bool WindowManager::createWindow() {
    window = new sf::RenderWindow(sf::VideoMode(width, height), title);
    if (!window) return false;
    
    window->setFramerateLimit(60);
    return true;
}

void WindowManager::closeWindow() {
    if (window && window->isOpen()) {
        window->close();
    }
}

bool WindowManager::isOpen() const {
    return window && window->isOpen();
}

void WindowManager::clear(const sf::Color& color) {
    if (window) {
        window->clear(color);
    }
}

void WindowManager::display() {
    if (window) {
        window->display();
    }
}

bool WindowManager::pollEvent(sf::Event& event) {
    if (window) {
        return window->pollEvent(event);
    }
    return false;
}
