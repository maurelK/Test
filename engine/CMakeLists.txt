cmake_minimum_required(VERSION 3.16)
project(RTypeEngine)

# -------------------------------------------
# Bibliothèque du moteur
# -------------------------------------------
add_library(rtype_engine STATIC
    # Core générique
    include/Core/GameEngine.hpp
    include/Core/EngineConfig.hpp
    include/Core/SceneManager.hpp
    src/Core/GameEngine.cpp
    
    # ECS générique
    include/ECS_architecture/base.hpp
    include/ECS_architecture/EntityManager.hpp
    include/ECS_architecture/Component_Manager.hpp
    include/ECS_architecture/System.hpp
    include/ECS_architecture/SystemManager.hpp
    include/ECS_architecture/Orchestror.hpp
    src/ECS_architecture/EntityManager.cpp
    
    # Ressources génériques
    include/Resources/ResourceManager.hpp
)

# Alias pour simplifier le lien dans le projet principal
add_library(engine ALIAS rtype_engine)

# -------------------------------------------
# Test standalone du moteur
# -------------------------------------------
#add_executable(engine_standalone_test
    #tests/engine_test.cpp
#)
#target_link_libraries(engine_standalone_test PRIVATE rtype_engine)

# -------------------------------------------
# Installation
# -------------------------------------------
install(TARGETS rtype_engine DESTINATION lib)
install(DIRECTORY include/ DESTINATION include/rtype_engine)