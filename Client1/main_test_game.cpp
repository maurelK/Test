#include "src/game/Game.hpp"

int main() {
    Game game(GameModeType::Solo);
    game.run();
    return 0;
}
