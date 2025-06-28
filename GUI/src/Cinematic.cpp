#include "Cinematic.hpp"

Cinematic::Cinematic() : isCinematicPlay(true) {}

void Cinematic::initialize()
{
    iTexture.loadFromFile("asserts/cinematic/the_boys.png");
    iTexture2.loadFromFile("asserts/cinematic/epi.png");
    iTexture3.loadFromFile("asserts/cinematic/load.png");
    image.setTexture(iTexture);
    image2.setTexture(iTexture2);
    image3.setTexture(iTexture3);

    centerSprite(image);
    centerSprite(image2);
    centerSprite(image3);
}

void Cinematic::centerSprite(sf::Sprite &sprite)
{
    sf::FloatRect bounds = sprite.getLocalBounds();
    sprite.setOrigin(bounds.width / 2, bounds.height / 2);
    sprite.setPosition(1920 / 2, 1080 / 2);
}

void Cinematic::animate(Window &window, Loading &loading)
{
    sf::Clock clock;

    window.clear();
    window.window.draw(image);
    window.display();
    sf::sleep(sf::seconds(1.5));

    window.clear();
    window.window.draw(image2);
    window.display();
    sf::sleep(sf::seconds(1.5));

    for (int i = 0; i < 100; ++i) {
        loading.update(i / 100.0f);
        window.clear();
        window.window.draw(image3);
        window.window.draw(loading.getSprite());
        window.display();
        sf::sleep(sf::milliseconds(10));
    }
}

void Cinematic::reset()
{
    isCinematicPlay = false;
}

void Cinematic::destroy()
{

}
