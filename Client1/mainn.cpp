#include <SFML/Graphics.hpp>

int main()
{
    sf::RenderWindow window(sf::VideoMode(800, 600), "Sprite");
    window.setFramerateLimit(60);

    sf::Texture texture;
    if (!texture.loadFromFile("assets/typeshi.png")) {
        return 1;
    }

    sf::Sprite sprite(texture);

    const int frameWidth = 64;
    const int frameHeight = 64;

    sprite.setTextureRect(sf::IntRect(0, 0, frameWidth, frameHeight));

    sprite.setPosition(
        (window.getSize().x / 2.f) - (frameWidth / 2.f),
        (window.getSize().y / 2.f) - (frameHeight / 2.f)
    );

    sprite.setScale(2.f, 2.f);

    int frame = 0;
    sf::Clock clock;

    while (window.isOpen())
    {
        sf::Event event;
        while (window.pollEvent(event))
        {
            if (event.type == sf::Event::Closed)
                window.close();
        }

        if (clock.getElapsedTime().asSeconds() > 0.1f)
        {
            frame = (frame + 1) % 8;
            sprite.setTextureRect(sf::IntRect(frame * frameWidth, 0, frameWidth, frameHeight));
            clock.restart();
        }

        window.clear();
        window.draw(sprite);
        window.display();
    }

    return 0;
}

