#include "View.hpp"

View::View(sf::RenderWindow &window) : window(window), zoomLevel(1.0f)
{
    view.setSize(1920, 1080);
    view.setCenter(960, 540);
}

void View::initialize()
{
    // Initialisation supplémentaire si nécessaire
}

void View::handleEvent(const sf::Event &event)
{
    if (event.type == sf::Event::KeyPressed)
    {
        float moveAmount = 10.0f;
        if (event.key.code == sf::Keyboard::Up)
        {
            zoomLevel *= 1.1f; // Zoom out
        }
        else if (event.key.code == sf::Keyboard::Down)
        {
            zoomLevel *= 0.9f; // Zoom in
        }
        else if (event.key.code == sf::Keyboard::Left)
        {
            view.move(-moveAmount, 0); // Move left
        }
        else if (event.key.code == sf::Keyboard::Right)
        {
            view.move(moveAmount, 0); // Move right
        }
        adjustView();
    }
}

void View::adjustView()
{
    view.setSize(1920 / zoomLevel, 1080 / zoomLevel);
    window.setView(view);
}

void View::update()
{
    // Mettre à jour la vue si nécessaire
}

void View::draw(sf::RenderWindow &window)
{
    // Dessiner des éléments spécifiques à la vue si nécessaire
}
