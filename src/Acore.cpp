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


void Acore::run(const std::string &graphicalLibPath, const std::string &gameLibPath)
{
    // === 1. Validation des librairies ===
    if (!isValidLibrary(graphicalLibPath, "createGraphical"))
    {
        std::cerr << "Error: '" << graphicalLibPath << "' is not a valid graphical library." << std::endl;
        exit(84);
    }

    if (!isValidLibrary(gameLibPath, "createGame"))
    {
        std::cerr << "Error: '" << gameLibPath << "' is not a valid game library." << std::endl;
        exit(84);
    }

    // === 2. Chargement dynamique ===
    void *graphicalHandle = dlopen(graphicalLibPath.c_str(), RTLD_LAZY);
    void *gameHandle = dlopen(gameLibPath.c_str(), RTLD_LAZY);

    if (!graphicalHandle || !gameHandle)
    {
        std::cerr << "Error (dlopen): " << dlerror() << std::endl;
        exit(84);
    }

    // === 3. Récupération des symboles ===
    auto createGraphical = reinterpret_cast<IGraphical *(*)()>(dlsym(graphicalHandle, "createGraphical"));
    auto deleteGraphical = reinterpret_cast<void (*)(IGraphical *)>(dlsym(graphicalHandle, "deleteGraphical"));
    auto createGame = reinterpret_cast<IGame *(*)()>(dlsym(gameHandle, "createGame"));
    auto deleteGame = reinterpret_cast<void (*)(IGame *)>(dlsym(gameHandle, "deleteGame"));

    if (!createGraphical || !deleteGraphical || !createGame || !deleteGame)
    {
        std::cerr << "Error (dlsym): Invalid library symbols." << std::endl;
        exit(84);
    }

    // === 4. Initialisation ===
    IGraphical *graphical = createGraphical();
    IGame *game = createGame();

    graphical->init();
    game->init();

    // === 5. Boucle principale ===
    while (true)
    {
        // Gestion des inputs
        int input = graphical->getInput();
        if (input == 'q')
            break; // Quitter le jeu

        // Mise à jour de la logique
        game->handleInput(input);
        game->update();

        // Rendu
        graphical->render(game->getRenderData());

        // Condition de fin de jeu
        if (game->getRenderData().shouldClose)
            break;
    }

    // === 6. Nettoyage ===
    deleteGraphical(graphical);
    deleteGame(game);
    dlclose(graphicalHandle);
    dlclose(gameHandle);
}

void Acore::runMenu(const std::string &defaultGraphicalLib)
{
    std::vector<std::string> gamesLibs;
    std::string graphicalLib;

    // === 1. Charger toutes les libs dans ./lib ===
    for (const auto &entry : std::filesystem::directory_iterator("./lib")) {
        const std::string filename = entry.path().string();
        if (filename.find(".so") == std::string::npos)
            continue;
        if (isValidLibrary(filename, "createGraphical"))
            graphicalLib = filename; // on garde la dernière (ou la seule)
        else if (isValidLibrary(filename, "createGame"))
            gamesLibs.push_back(filename);
    }

    if (graphicalLib.empty() || gamesLibs.empty()) {
        std::cerr << "Error: No valid graphical or game libraries found.\n";
        return;
    }

    // === 2. Charger la lib graphique ===
    void *graphicHandle = dlopen(graphicalLib.c_str(), RTLD_LAZY);
    if (!graphicHandle) {
        std::cerr << "Error (dlopen): " << dlerror() << std::endl;
        return;
    }

    auto createGraphical = reinterpret_cast<IGraphical *(*)()>(dlsym(graphicHandle, "createGraphical"));
    auto deleteGraphical = reinterpret_cast<void (*)(IGraphical *)>(dlsym(graphicHandle, "deleteGraphical"));
    if (!createGraphical || !deleteGraphical) {
        std::cerr << "Error: invalid graphical lib symbols\n";
        dlclose(graphicHandle);
        return;
    }

   
    // === 4. Lancer le jeu ===
    run(graphicalLib, chosenGame);
}