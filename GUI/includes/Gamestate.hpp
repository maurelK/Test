#ifndef GAMESTATE_HPP
#define GAMESTATE_HPP

#include <vector>
#include <string>
#include <map>


struct Tile {
    int x, y;
    int resources[7];
};


struct Player {
    int id;
    int x, y;
    int orientation;
    int level;
    std::string teamName;
};

struct Team {
    std::string name;
    int nbDrones;
    int nbMatureEggs;
};

struct GameState {
    int width;
    int height;
    std::vector<std::vector<Tile>> map;
    std::map<int, Player> players;
    std::map<std::string, Team> teams;
};

#endif
