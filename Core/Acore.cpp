#include "Acore.hpp"
#include <dlfcn.h>
#include <iostream>
#include <unistd.h>
#include <dirent.h>
#include <sys/stat.h>
#include <algorithm>
int Acore::libRead(const std::string &path) {
    DIR *dir = opendir(path.c_str());
    if (!dir) {
        std::cerr << "Error: Could not open directory: " << path << std::endl;
        return 84;
    }

    struct dirent *ent;
    while ((ent = readdir(dir)) != nullptr) {
        std::string filename = ent->d_name;
        if (filename.size() > 3 && filename.substr(filename.size() - 3) == ".so") {
            std::string fullPath = path + "/" + filename;
            
            if (isGraphicalLib(fullPath)) {
                Graphics_lib.push_back(fullPath);
            } else if (isGameLib(fullPath)) {
                Games_lib.push_back(fullPath);
            } else {
                std::cerr << "Warning: " << filename << " is not a valid game or graphics library" << std::endl;
            }
        }
    }
    closedir(dir);

    if (Graphics_lib.empty()) {
        std::cerr << "Error: No valid graphics libraries found in " << path << std::endl;
        return 84;
    }

    if (Games_lib.empty()) {
        std::cerr << "Error: No valid game libraries found in " << path << std::endl;
        return 84;
    }

    return 0;
}

bool Acore::isGraphicalLib(const std::string &filename)
{
    void *handle = dlopen(filename.c_str(), RTLD_LAZY);
    if (!handle) {
        std::cerr << "dlopen failed for " << filename << ": " << dlerror() << std::endl;
        return false;
    }

    void *sym = dlsym(handle, "createGraphicalInstance");
    if (!sym) {
        std::cerr << "dlsym failed for " << filename << ": " << dlerror() << std::endl;
        dlclose(handle);
        return false;
    }

    dlclose(handle);
    return true;
}

bool Acore::isGameLib(const std::string &filename) {
    void *handle = dlopen(filename.c_str(), RTLD_LAZY);
    if (!handle) {
        std::cerr << "dlopen failed for " << filename << ": " << dlerror() << std::endl;
        return false;
    }

    void *sym = dlsym(handle, "createGameInstance");
    if (!sym) {
        std::cerr << "dlsym failed for " << filename << ": " << dlerror() << std::endl;
        dlclose(handle);
        return false;
    }

    dlclose(handle);
    return true;
}

template <typename T>
std::pair<std::unique_ptr<T>, void*> Acore::libLoading(const std::string &lib, const std::string &symbol) {
    void *handle = dlopen(lib.c_str(), RTLD_NOW | RTLD_GLOBAL);
    if (!handle) {
        std::cerr << "Error: dlopen failed for " << lib << ": " << dlerror() << std::endl;
        return {nullptr, nullptr};
    }

    dlerror(); // Clear existing errors
    auto createFunc = reinterpret_cast<T*(*)()>(dlsym(handle, symbol.c_str()));
    
    if (const char* error = dlerror()) {
        std::cerr << "Error: dlsym failed for symbol " << symbol << " in " << lib << ": " << error << std::endl;
        dlclose(handle);
        return {nullptr, nullptr};
    }

    try {
        return {std::unique_ptr<T>(createFunc()), handle};
    } catch (const std::exception& e) {
        std::cerr << "Error: Factory function failed for " << lib << ": " << e.what() << std::endl;
        dlclose(handle);
        return {nullptr, nullptr};
    }
}

int Acore::RunMenu(std::string default_lib) {
    // Validate default library exists
    auto it = std::find(Graphics_lib.begin(), Graphics_lib.end(), default_lib);
    if (it == Graphics_lib.end()) {
        std::cerr << "Error: Specified library not found: " << default_lib << std::endl;
        return 84;
    }

    auto [graphic, handle] = libLoading<IGraphical>(default_lib, "createGraphicalInstance");
    if (!graphic) {
        std::cerr << "Error: Failed to load graphical library" << std::endl;
        return 84;
    }

    try {
        graphic->init();
        MenuChoice choice;
        choice.playerName = graphic->getPlayerName();
        choice.selectedGraphic = default_lib;
        choice.selectedGame = graphic->displayMenu(Games_lib);

        if (choice.selectedGame.empty()) {
            std::cerr << "Error: No game selected" << std::endl;
            dlclose(handle);
            return 84;
        }

        runGame(choice);
    } catch (const std::exception& e) {
        std::cerr << "Exception: " << e.what() << std::endl;
        dlclose(handle);
        return 84;
    }

    dlclose(handle);
    return 0;
}


void Acore::runGame(const MenuChoice& choice) {
    auto [graphic, graphicHandle] = libLoading<IGraphical>(choice.selectedGraphic, "createGraphicalInstance");
    auto [game, gameHandle] = libLoading<IGame>(choice.selectedGame, "createGameInstance");

    if (!graphic || !game) {
        std::cerr << "Error: Failed to load required libraries" << std::endl;
        return;
    }

    graphic->init();
    game->init();

    bool running = true;
    while (running) {
        int input = graphic->getInput();
        if (input == 'q') {
            running = false;
        } else {
            game->handleInput(input);
        }

        game->update();
        graphic->clear();
        graphic->draw(game->getDisplay());
        graphic->refresh();

        if (game->isGameOver()) {
            running = false;
        }
    }

    // Save score here
    dlclose(gameHandle);
    dlclose(graphicHandle);
}

//void Acore::saveScore(const std::string& game, const std::string& player, int score) {
//    std::filesystem::create_directory("scores");
//    std::ofstream file("scores/" + game + ".scores", std::ios::app);
//    
//    if (file.is_open()) {
//        file << player << ":" << score << "\n";
//    } else {
//        std::cerr << "Warning: Failed to save score for " << game << std::endl;
//    }
//}
//
//void Acore::loadScores() {
//    if (!std::filesystem::exists("scores")) return;
//
//    for (const auto& entry : std::filesystem::directory_iterator("scores")) {
//        if (entry.path().extension() == ".scores") {
//            std::ifstream file(entry.path());
//            std::string gameName = entry.path().stem().string();
//            std::string line;
//
//            while (std::getline(file, line)) {
//                size_t pos = line.find(':');
//                if (pos != std::string::npos) {
//                    std::string player = line.substr(0, pos);
//                    int score = std::stoi(line.substr(pos + 1));
//                    scores[gameName].push_back(score);
//                }
//            }
//        }
//    }
//}

int main(int ac, char **av)
{
    if (ac != 2) {
        std::cerr << "Usage: " << av[0] << " <graphics_library_path>" << std::endl;
        return 84;
    }

    std::string libPath = av[1];
    FILE* file = fopen(libPath.c_str(), "r");
    if (!file) {
        std::cerr << "Error: Library not found: " << libPath << std::endl;
        return 84;
    }
    fclose(file);

    Acore acore;
    int status = acore.libRead("lib");
    if (status == 84) {
        return 84;
    }

    return acore.RunMenu(libPath);
}
