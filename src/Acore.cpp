#include "../include/Acore.hpp"
#include "../include/IGraphical.hpp"
#include "../include/IGame.hpp"
#include <iostream>
#include <stdexcept>

Acore::Acore() {}
Acore::~Acore() {}

bool Acore::isValidLibrary(const std::string &path, const std::string &symbol)
{
    void *handle = dlopen(path.c_str(), RTLD_LAZY);
    if (!handle)
    {
        std::cerr << "Error (dlopen): " << dlerror() << std::endl;
        return false;
    }

    void *sym = dlsym(handle, symbol.c_str());
    dlclose(handle);
    return sym != nullptr;
}

void Acore::loadAvailableLibs()
{
    this->state.gameLibs.clear();
    this->state.graphicLibs.clear();

    for (const auto &entry : std::filesystem::directory_iterator("./lib")) {
        const std::string path = entry.path().string();
        
        if (isValidLibrary(path, "createGraphical")) {
            this->state.graphicLibs.push_back(path);
        }
        else if (isValidLibrary(path, "createGame")) {
            this->state.gameLibs.push_back(path);
        }
    }

    std::sort(this->state.gameLibs.begin(), this->state.gameLibs.end());
    std::sort(this->state.graphicLibs.begin(), this->state.graphicLibs.end());
}

void Acore::switchGraphicalLib(const std::string& newLib, void*& handle, IGraphical*& graphical) {
    if (handle) dlclose(handle);
    
    handle = dlopen(newLib.c_str(), RTLD_LAZY);
    if (!handle) {
        std::cerr << "Failed to load graphical lib: " << dlerror() << std::endl;
        return;
    }

    auto create = reinterpret_cast<IGraphical*(*)()>(dlsym(handle, "createGraphical"));
    graphical = create();
    graphical->init();
}

void Acore::runMenu(const std::string& initialLib)
{
    if (!isValidLibrary(initialLib, "createGraphical")) {
        std::cerr << "Error: '" << initialLib << "' is not a valid graphical library.\n";
        exit(84);
    }

    this->loadAvailableLibs();
    this->loadScores();

    void* graphicHandle = dlopen(initialLib.c_str(), RTLD_LAZY);
    auto createGraphical = reinterpret_cast<IGraphical*(*)()>(dlsym(graphicHandle, "createGraphical"));
    IGraphical* graphical = createGraphical();
    graphical->init();

    bool running = true;
    while (running) {
        IGraphical::RenderData renderData;
        updateMenuRender(renderData);
        graphical->render(renderData);

        int input = graphical->getInput();
        handleGlobalInput(input, graphicHandle, graphical);
    }

    dlclose(graphicHandle);
    delete graphical;
}

void Acore::handleGlobalInput(int input, void*& handle, IGraphical*& graphical)
{
    switch (input) {
        case 27: // ESC
            exit(0);
        case 'n':
    if (!this->state.graphicLibs.empty()) {
        this->state.selectedGraphic = (this->state.selectedGraphic + 1) % this->state.graphicLibs.size();
        this->switchGraphicalLib(this->state.graphicLibs[this->state.selectedGraphic], handle, graphical);
            }
            break;
        case KEY_DOWN:
            this->state.selectedGame = (this->state.selectedGame + 1) % this->state.gameLibs.size();
            break;
        case KEY_UP:
            this->state.selectedGame = (this->state.selectedGame - 1 + this->state.gameLibs.size()) % this->state.gameLibs.size();
            break;
    }
}

void Acore::updateMenuRender(IGraphical::RenderData& renderData)
{
    // Clear existing render data
    renderData.entities.clear();
    renderData.texts.clear();

    // Title
    renderData.texts.push_back(IGraphical::GameText{10, 2, /*content*/ "ARCADE MENU", 1});

    // Games list
    for (size_t i = 0; i < this->state.gameLibs.size(); ++i) {
        std::string name = std::filesystem::path(this->state.gameLibs[i]).stem().string();
        name = name.substr(name.find('_') + 1); // Remove lib prefix
        renderData.texts.push_back(IGraphical::GameText{15, 5 + (int)i, name, (i == this->state.selectedGame) ? 2 : 3});
    }

    // Graphics list
    for (size_t i = 0; i < this->state.graphicLibs.size(); ++i) {
        std::string name = std::filesystem::path(this->state.graphicLibs[i]).stem().string();
        name = name.substr(name.find('_') + 1); // Remove lib prefix
        renderData.texts.push_back(IGraphical::GameText{40, 5 + (int)i, name, (i == this->state.selectedGraphic) ? 2 : 3});
    }

    // Player name
    renderData.texts.push_back(IGraphical::GameText{10, 20, "Player: " + this->state.playerName, 1});

    // Scores
    for (size_t i = 0; i < std::min(this->state.scores.size(), (size_t)5); ++i) {
        renderData.texts.push_back(IGraphical::GameText{40, 20 + (int)i, /*content*/ 
                                 this->state.scores[i].first + ": " + 
                                 std::to_string(this->state.scores[i].second), 4});
    }
}

void Acore::saveScore(const std::string& game, int score)
{
    std::ofstream file("scores.txt", std::ios::app);
    if (file) {
        file << state.playerName << ";" << game << ";" << score << "\n";
    }
}

void Acore::loadScores()
{
    std::ifstream file("scores.txt");
    if (file) {
        std::string line;
        while (std::getline(file, line)) {
            // Parsing du format: nom;jeu;score
            size_t pos1 = line.find(';');
            size_t pos2 = line.find(';', pos1+1);
            if (pos1 != std::string::npos && pos2 != std::string::npos) {
                std::string name = line.substr(0, pos1);
                std::string game = line.substr(pos1+1, pos2-pos1-1);
                int score = std::stoi(line.substr(pos2+1));
                state.scores.emplace_back(name + " (" + game + ")", score);
            }
        }
    }
    std::sort(this->state.scores.begin(), this->state.scores.end(), 
            [](auto& a, auto& b) { return a.second > b.second; });
}