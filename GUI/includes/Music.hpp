#ifndef MUSIC_HPP
#define MUSIC_HPP

#include <SFML/Audio.hpp>

class Music
{
public:
    Music();
    void initialize();
    void adjustVolume(int volumeChange);
    void play();
    void stop();

private:
    sf::Music music;
    int volume;
};

#endif