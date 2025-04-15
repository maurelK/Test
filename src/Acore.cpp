#include "../include/Acore.hpp"
#include "../include/IGraphical.hpp"
#include "../include/IGame.hpp"
#include <iostream>
#include <stdexcept>

Acore::Acore() : state{0, 0, {}, {}, {}, ""} {}
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
    // Clean up previous library
    if (graphical) {
        graphical->close();
        delete graphical;
        graphical = nullptr;
    }
    if (handle) {
        dlclose(handle);
        handle = nullptr;
    }

    // Load new library
    handle = dlopen(newLib.c_str(), RTLD_LAZY);
    if (!handle) {
        std::cerr << "Failed to load graphical lib: " << dlerror() << std::endl;
        return;
    }

    // Create new instance
    auto create = reinterpret_cast<IGraphical*(*)()>(dlsym(handle, "createGraphical"));
    if (!create) {
        std::cerr << "Failed to find createGraphical symbol: " << dlerror() << std::endl;
        dlclose(handle);
        return;
    }

    graphical = create();
    graphical->init();
}





























//void Acore::switchGraphicalLib(const std::string& newLib, void*& handle, IGraphical*& graphical) {
//    // Clean up previous library
//    if (graphical) {
//        graphical->close();
//        delete graphical;
//        graphical = nullptr;
//    }
//    if (handle) {
//        dlclose(handle);
//        handle = nullptr;
//    }
//
//    // Load new library
//    handle = dlopen(newLib.c_str(), RTLD_LAZY);
//    if (!handle) {
//        std::cerr << "Failed to load graphical lib: " << dlerror() << std::endl;
//        return;
//    }
//
//    // Create new instance
//    auto create = reinterpret_cast<IGraphical*(*)()>(dlsym(handle, "createGraphical"));
//    if (!create) {
//        std::cerr << "Failed to find createGraphical symbol: " << dlerror() << std::endl;
//        dlclose(handle);
//        return;
//    }
//
//    graphical = create();
//    if (!graphical->init()) {
//        std::cerr << "Failed to initialize graphical library" << std::endl;
//        delete graphical;
//        dlclose(handle);
//        graphical = nullptr;
//        handle = nullptr;
//    } else {
//        std::cout << "Successfully switched to: " << newLib << std::endl;
//    }
//}

int Acore::runMenu(const std::string& initialLib) {
    if (!isValidLibrary(initialLib, "createGraphical")) {
        std::cerr << "Error: '" << initialLib << "' is not a valid graphical library.\n";
        exit(84);
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
        if (currentGame) {
            // Boucle de jeu
            int input = graphical->getInput();
            if (input == 27) { // ESC key or SDL_QUIT
                running = false;
                continue;
            }
            currentGame->handleInput(input);
            currentGame->update();
            const auto& gameRenderData = currentGame->getRenderData();
            
            // Rendu du jeu
            IGraphical::RenderData renderData;

            // Convertir les entités
            for (const auto& entity : gameRenderData.entities) {
                renderData.entities.push_back({
                    entity.x,
                    entity.y,
                    1,  // width
                    1,  // height
                    entity.color,
                    entity.symbol,
                    "",  // spritePath
                    false // useSprite
                });
            }
            
            // Convertir les textes
            for (const auto& text : gameRenderData.texts) {
                renderData.texts.emplace_back(
                    text.x,
                    text.y,
                    text.content,
                    text.color
                );
            }
            
            graphical->render(renderData);
            

            if (gameRenderData.shouldClose) {
                unloadGame();
            }
        } else {
            // Boucle du menu
            IGraphical::RenderData renderData;
            updateMenuRender(renderData);
            graphical->render(renderData);
            int input = graphical->getInput();
            if (input == 27) { // ESC key or SDL_QUIT
                running = false;
            } else {
                handleGlobalInput(input, graphicHandle, graphical);
            }
        }
    }

    unloadGame(); // Ensure game is unloaded before cleanup

    graphical->close();
    dlclose(graphicHandle);
    delete graphical;
    return 0;
}

void Acore::updateMenuRender(IGraphical::RenderData& renderData)
{
    renderData.entities.clear();
    renderData.texts.clear();

    renderData.texts.emplace_back(35, 1, "ARCADE MENU", 2);


    renderData.texts.emplace_back(5, 3, "----- GAMES -----", 3);
    renderData.texts.emplace_back(45, 3, "-- GRAPHICAL LIBS --", 3);

    
    for (size_t i = 0; i < state.gameLibs.size(); ++i) {
        std::string name = std::filesystem::path(state.gameLibs[i]).stem().string();
        std::string display = (i == state.selectedGame) ? "> " + name : "  " + name;
        renderData.texts.emplace_back(5, 5 + (int)i, display, (i == state.selectedGame) ? 4 : 1);
    }

    
    for (size_t i = 0; i < state.graphicLibs.size(); ++i) {
        std::string name = std::filesystem::path(state.graphicLibs[i]).stem().string();
        std::string display = (i == state.selectedGraphic) ? "> " + name : "  " + name;
        renderData.texts.emplace_back(45, 5 + (int)i, display, (i == state.selectedGraphic) ? 4 : 1);
    }

    
    renderData.texts.emplace_back(5, 15, "PLAYER: " + state.playerName, 5);
    renderData.texts.emplace_back(45, 15, "TOP SCORES", 2);
    for (size_t i = 0; i < std::min(state.scores.size(), (size_t)5); ++i) {
        std::string score = std::to_string(i + 1) + ". " + state.scores[i].first + " - " + std::to_string(state.scores[i].second);
        renderData.texts.emplace_back(45, 17 + (int)i, score, 3);
    }

    
    renderData.texts.emplace_back(5, 23, "UP/DOWN: Select game", 1);
    renderData.texts.emplace_back(5, 24, "LEFT/RIGHT: Select lib", 1);
    renderData.texts.emplace_back(5, 25, "ENTER: Launch game", 1);
    renderData.texts.emplace_back(5, 26, "N: Switch graphical lib", 1);
    renderData.texts.emplace_back(5, 27, "ESC: Quit", 1);
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

std::string Acore::getLibName(const std::string& path) const {
    size_t start = path.find_last_of("/\\") + 1;
    size_t end = path.find_last_of('.');
    return path.substr(start, end - start);
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

void Acore::loadGame(const std::string& path) {
    // Fermer le jeu actuel s'il existe
    unloadGame();

    std::cout << "Loading game from: " << path << std::endl;

    currentGameHandle = dlopen(path.c_str(), RTLD_LAZY);
    if (!currentGameHandle) {
        std::cerr << "Error loading game library: " << dlerror() << std::endl;
        return;
    }
    auto createGame = reinterpret_cast<IGame*(*)()>(dlsym(currentGameHandle, "createGame"));
    if (!createGame) {
        std::cerr << "Error: Missing createGame symbol: " << dlerror() << std::endl;
        dlclose(currentGameHandle);
        return;
    }

    currentGame = createGame();
    if (!currentGame) {
        std::cerr << "Error: Game creation failed" << std::endl;
        dlclose(currentGameHandle);
        return;
    }

    try {
        currentGame->init();
        std::cout << "Game initialized successfully" << std::endl;
    } catch (const std::exception& e) {
        std::cerr << "Error initializing game: " << e.what() << std::endl;
        delete currentGame;
        dlclose(currentGameHandle);
        currentGame = nullptr;
        currentGameHandle = nullptr;
    }
}

void Acore::unloadGame() {
    if (currentGame) {
        delete currentGame;
        currentGame = nullptr;
    }
    if (currentGameHandle) {
        dlclose(currentGameHandle);
        currentGameHandle = nullptr;
    }
}

void Acore::handleGlobalInput(int input, void*& handle, IGraphical*& graphical) {
    std::cout << "Current selected graphic lib: " << this->state.graphicLibs[this->state.selectedGraphic] << std::endl; // Debug statement
    std::cout << "Current selected game: " << this->state.gameLibs[this->state.selectedGame] << std::endl; // Debug statement
    switch (input) {
        case 27:
            // exit(EXIT_SUCCESS);
            // Do nothing here, handled in runMenu loop
            break;
            
        case KEY_DOWN:
            state.selectedGame = (state.selectedGame + 1) % state.gameLibs.size();
            break;
            
        case KEY_UP:
            state.selectedGame = (state.selectedGame - 1 + state.gameLibs.size()) % state.gameLibs.size();
            break;
            
        case KEY_RIGHT:
            state.selectedGraphic = (state.selectedGraphic + 1) % state.graphicLibs.size();
            break;
            
        case KEY_LEFT:
            state.selectedGraphic = (state.selectedGraphic - 1 + state.graphicLibs.size()) % state.graphicLibs.size();
            break;
            
        case 10:
            if (!state.gameLibs.empty() || !state.graphicLibs.empty()) {
                switchGraphicalLib(state.graphicLibs[state.selectedGraphic], handle, graphical);
                loadGame(state.gameLibs[state.selectedGame]);

            }
            break;
            
        case 'n':
            if (!state.graphicLibs.empty()) {
                switchGraphicalLib(state.graphicLibs[state.selectedGraphic], handle, graphical);
            }
            break;
        default:
            //if (isalnum(input) || input == ' ') {
            //    handleNameInput(input);
            //}
            break;
    }
}
