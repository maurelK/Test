#include "Menu.hpp"
#include <iostream>

Menu::Menu() : selectedOptionIndex(0)
{
    if (!font.loadFromFile("asserts/font/Race Sport.ttf"))
    {
        std::cerr << "Failed to load font" << std::endl;
    }
    setupTextOptions();
}

void Menu::setupTextOptions()
{
    std::vector<std::string> optionStrings = {"Play", "Settings", "Exit"};

    for (size_t i = 0; i < optionStrings.size(); ++i)
    {
        sf::Text text;
        text.setFont(font);
        text.setString(optionStrings[i]);
        text.setCharacterSize(50);
        text.setFillColor(sf::Color::White);
        text.setPosition(960 - text.getLocalBounds().width / 2, 500 + i * 100);
        options.push_back(text);
    }

    if (!options.empty())
    {
        options[selectedOptionIndex].setFillColor(sf::Color::Yellow);
    }
}

void Menu::draw(sf::RenderWindow &window)
{
    for (const auto &option : options)
    {
        window.draw(option);
    }
}

void Menu::handleEvent(const sf::Event &event, bool &shouldStartGame)
{
    if (event.type == sf::Event::KeyPressed)
    {
        if (event.key.code == sf::Keyboard::Up)
        {
            moveUp();
        }
        else if (event.key.code == sf::Keyboard::Down)
        {
            moveDown();
        }
        else if (event.key.code == sf::Keyboard::Enter)
        {
            if (selectedOptionIndex == 0)
            {
                shouldStartGame = true;
            }
            else if (selectedOptionIndex == 2)
            {
                exit(0);
            }
        }
    }
}

void Menu::moveUp()
{
    options[selectedOptionIndex].setFillColor(sf::Color::White);
    selectedOptionIndex = (selectedOptionIndex - 1 + options.size()) % options.size();
    options[selectedOptionIndex].setFillColor(sf::Color::Yellow);
}

void Menu::moveDown()
{
    options[selectedOptionIndex].setFillColor(sf::Color::White);
    selectedOptionIndex = (selectedOptionIndex + 1) % options.size();
    options[selectedOptionIndex].setFillColor(sf::Color::Yellow);
}
