#include "../include/Acore.hpp"
#include "../include/IGraphical.hpp"
#include "../include/IGame.hpp"
#include <iostream>
#include <stdexcept>

Acore::Acore() :state{0, 0, {}, {}, {}, ""} {}
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
    void* newHandle = dlopen(newLib.c_str(), RTLD_LAZY | RTLD_NODELETE);
    if (!newHandle) {
        std::cerr << "Failed to load graphical lib: " << dlerror() << std::endl;
        return;
    }

    auto create = reinterpret_cast<IGraphical*(*)()>(dlsym(newHandle, "createGraphical"));
    if (!create) {
        std::cerr << "dlsym error: " << dlerror() << std::endl;
        dlclose(newHandle);
        return;
    }

    IGraphical* newGraphical = create();
    /*if (!newGraphical->init()) {
        std::cerr << "Failed to initialize graphical library" << std::endl;
        delete newGraphical;
        dlclose(newHandle);
        return;
    }*/

    if (handle) {
        graphical->close();
        dlclose(handle);
    }
    
    handle = newHandle;
    graphical = newGraphical;
}

void Acore::runMenu(const std::string& initialLib) {
    if (!isValidLibrary(initialLib, "createGraphical")) {
        std::cerr << "Error: '" << initialLib << "' is not a valid graphical library.\n";
        exit(EXIT_FAILURE);
    }

    loadAvailableLibs();
    loadScores();

    void* graphicHandle = nullptr;
    IGraphical* graphical = nullptr;
    switchGraphicalLib(initialLib, graphicHandle, graphical);

    if (!graphical || !graphicHandle) {
        std::cerr << "Failed to initialize initial graphical library" << std::endl;
        exit(EXIT_FAILURE);
    }

    // Get player name before main loop
    if (state.playerName.empty()) {
        state.playerName = graphical->getPlayerName();
    }

    bool running = true;
    while (running) {
        IGraphical::RenderData renderData;
        updateMenuRender(renderData);
        graphical->render(renderData);

        int input = graphical->getInput();
        handleGlobalInput(input, graphicHandle, graphical);
    }

    graphical->close();
    dlclose(graphicHandle);
    delete graphical;
}

void Acore::updateMenuRender(IGraphical::RenderData& renderData)
{
    renderData.entities.clear();
    renderData.texts.clear();

    renderData.texts.push_back(IGraphical::GameText{10, 2, "-----------------------------ARCADE MENU------------------------------------", 1});

    //game list my gee
    for (size_t i = 0; i < this->state.gameLibs.size(); ++i) {
        std::string name = std::filesystem::path(this->state.gameLibs[i]).stem().string();
        renderData.texts.push_back(IGraphical::GameText{15, 5 + (int)i, name, (i == this->state.selectedGame) ? 2 : 3});
    }

    // Graphics list also gee
    for (size_t i = 0; i < this->state.graphicLibs.size(); ++i) {
        std::string name = std::filesystem::path(this->state.graphicLibs[i]).stem().string();
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

void Acore::handleNameInput(int input)
{
    if (input == 127 && !state.playerName.empty()) { // Backspace
        state.playerName.pop_back();
    }
    else if (isalnum(input)) {
        state.playerName += static_cast<char>(input);
    }
}

void Acore::handleGlobalInput(int input, void*& handle, IGraphical*& graphical) {
    switch (input) {
        case 27: // ESC
            exit(EXIT_SUCCESS);
        case 'n':
            if (!state.graphicLibs.empty()) {
                state.selectedGraphic = (state.selectedGraphic + 1) % state.graphicLibs.size();
                switchGraphicalLib(state.graphicLibs[state.selectedGraphic], handle, graphical);
            }
            break;
        case KEY_DOWN:
            state.selectedGame = (state.selectedGame + 1) % state.gameLibs.size();
            break;
        case KEY_UP:
            state.selectedGame = (state.selectedGame - 1 + state.gameLibs.size()) % state.gameLibs.size();
            break;
        case 127: // Backspace
        case KEY_BACKSPACE:
            handleNameInput(input);
            break;
        default:
            if (isalnum(input)) {
                handleNameInput(input);
            }
            break;
    }
}
