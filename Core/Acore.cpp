#include "Acore.hpp"

int Acore::libRead(const std::string &path)
{
    DIR *dir;
    struct dirent *ent;

    dir = opendir(path.c_str());
    if (!dir) {
        std::cerr << "Could not open directory: " << path << std::endl;
        return 84;
    }
    while((ent = readdir(dir)) != NULL) {
        std::string filename;
        filename = ent->d_name;
        std::string filename_comp;
        //if (filename.substr(filename.size() - 3) == ".so") {
        if (filename.size() > 3 && filename.substr(filename.size() - 3) == ".so") {
            filename_comp = path + "/" + filename;
        
            if (Graphical_lib(filename_comp) == true) {
                Graphics_lib.push_back(filename_comp);
            } else if (Game_lib(filename_comp) == true) {
                Games_lib.push_back(filename_comp);
            }
        }
    }
    closedir(dir);
    return 0;
}

bool Acore::Graphical_lib(const std::string &filename_comp)
{
    void *handle = dlopen(filename_comp.c_str(), RTLD_LAZY);
    if (!handle) {
        return false;
    }
    void *sym = dlsym(handle, "createInstance");
    if (!sym) {
        dlclose(handle);
        return false;
    }
    dlclose(handle);
    return true;
}

bool Acore::Game_lib(const std::string &filename_comp)
{
    void *handle = dlopen(filename_comp.c_str(), RTLD_LAZY);
    if (!handle) {
        return false;
    }
    void *sym = dlsym(handle, "createGameInstance");
    if (!sym) {
        dlclose(handle);
        return false;
    }
    dlclose(handle);
    return true;
}

// Improved libLoading in Acore.cpp
template <typename T>
std::pair<T*, void*> Acore::libLoading(const std::string &lib, const std::string &symbol)
{
    void *handle = dlopen(lib.c_str(), RTLD_LAZY);
    if (!handle) {
        std::cerr << "dlopen error: " << dlerror() << std::endl;
        return {nullptr, nullptr};
    }

    dlerror(); // Clear any existing error
    typedef T* (*CreateFunc)();
    CreateFunc createFunc = (CreateFunc)dlsym(handle, symbol.c_str());
    
    const char *dlsym_error = dlerror();
    if (dlsym_error) {
        std::cerr << "dlsym error: " << dlsym_error << std::endl;
        dlclose(handle);
        return {nullptr, nullptr};
    }

    return {createFunc(), handle};
}

int Acore::RunMenu(std::string default_lib)
{
    if (Graphics_lib.empty()) {
        std::cerr << "No graphical libraries found." << std::endl;
        return 84;
    }

    // Vérifier si la bibliothèque par défaut est dans la liste des bibliothèques graphiques disponibles
    bool found = false;
    for (const auto &lib : Graphics_lib) {
        if (lib == default_lib) {
            found = true;
            break;
        }
    }

    if (!found) {
        std::cerr << "Graphical library " << default_lib << " not found, using the first available one." << std::endl;
        // Utiliser la première bibliothèque graphique disponible
        default_lib = Graphics_lib[0];
    }

    IGraphical *graphic = libLoading<IGraphical>(default_lib, "createInstance");

    if (!graphic) {
        std::cerr << "Failed to load graphical lib: " << default_lib << std::endl;
        return 84;
    }

    // Demander le nom du joueur
    std::string playerName = graphic->getPlayerName();

    // Afficher le menu et sélectionner un jeu
    std::string selectedGame = graphic->displayMenu(Games_lib);

    // Sauvegarder les choix dans MenuChoice
    MenuChoice choice;
    choice.selectedGame = selectedGame;
    choice.selectedGraphic = default_lib;
    choice.playerName = playerName;

    // Afficher les informations sélectionnées
    std::cout << "Chosen Game: " << choice.selectedGame << std::endl;
    std::cout << "Chosen Graphic Mode: " << choice.selectedGraphic << std::endl;
    std::cout << "Player: " << choice.playerName << std::endl;

    return 0;
}


int main(int ac, char **av)
{
    if (ac < 2) {
        std::cerr << "Na be two arguments my gee\n";
        return 84;
    }
    Acore acore;
    int retour = acore.libRead("lib");
    if (retour == 84) {
        exit(84);
    }
    return(acore.RunMenu(av[1]));
}