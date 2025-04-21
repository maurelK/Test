#ifndef ARCADE_SFML_HPP
    #define ARCADE_SFML_HPP

    #include <SFML/Graphics.hpp>
    #include <string>
    #include <vector>
    #include "IGraphical.hpp"

class ArcadeSFML : public IGraphical {
public:
    ArcadeSFML();
    ~ArcadeSFML();

    bool init() override;
    void close() override;
    void render(const RenderData &data) override;
    int getInput() override;
    std::string getPlayerName() override;

private:
    sf::RenderWindow *window;
    sf::Font font;

    void renderText(const std::string &text, int x, int y, sf::Color color);
    sf::Color getColorFromCode(int colorCode);
};

extern "C" {
    IGraphical *createGraphical();
    void deleteGraphical(IGraphical *graphical);
}

#endif