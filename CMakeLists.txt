# MOTEUR DE JEU GÉNÉRIQUE - COMPILE SEUL
cmake_minimum_required(VERSION 3.16)
project(RTypeEngine)

# Dépendances EXTERNES seulement - SANS TOUTES LES COMPOSANTES SFML
# find_package(SFML 2.5 COMPONENTS System REQUIRED)

# 📦 BIBLIOTHÈQUE DU ENGINE
add_library(rtype_engine STATIC
    # CORE GÉNÉRIQUE
    include/Core/GameEngine.hpp
    include/Core/EngineConfig.hpp
    include/Core/SceneManager.hpp
    src/Core/GameEngine.cpp
    
    # ECS GÉNÉRIQUE
    include/ECS_architecture/base.hpp
    include/ECS_architecture/EntityManager.hpp
    include/ECS_architecture/Component_Manager.hpp
    include/ECS_architecture/System.hpp
    include/ECS_architecture/SystemManager.hpp
    include/ECS_architecture/Orchestror.hpp
    src/ECS_architecture/EntityManager.cpp
    
    # RESSOURCES GÉNÉRIQUES
    include/Resources/ResourceManager.hpp
)

# Lier seulement les dépendances disponibles
# target_link_libraries(rtype_engine
#     sfml-system
#     # sfml-network     # ⭐ RETIRÉ - pas disponible
#     # sfml-graphics    # ⭐ RETIRÉ - pas disponible
#     # sfml-window      # ⭐ RETIRÉ - pas disponible
#     # sfml-audio       # ⭐ RETIRÉ - pas disponible
# )

# 🧪 EXÉCUTABLE DE TEST DU ENGINE SEUL
add_executable(engine_standalone_test
    tests/engine_test.cpp
)
target_link_libraries(engine_standalone_test rtype_engine)

# INSTALLATION (pour usage par R-Type)
install(TARGETS rtype_engine DESTINATION lib)
install(DIRECTORY include/ DESTINATION include/rtype_engine)