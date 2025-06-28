#include "Music.hpp"

Music::Music() : volume(50) {}

void Music::initialize()
{
    music.openFromFile("asserts/music/music_menu.ogg");
    music.setVolume(volume);
    music.setLoop(true);
    play();
}

void Music::adjustVolume(int volumeChange)
{
    volume += volumeChange;
    if (volume < 0)
        volume = 0;
    if (volume > 100)
        volume = 100;
    music.setVolume(volume);
}

void Music::play()
{
    music.play();
}

void Music::stop()
{
    music.stop();
}
