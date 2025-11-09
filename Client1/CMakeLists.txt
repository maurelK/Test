cmake_minimum_required(VERSION 3.15)
project(RTypeClient VERSION 1.0.0 LANGUAGES CXX)

# Standard C++20
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

add_compile_options(-include utility)

# Options de compilation selon la plateforme
if(MSVC)
    # Windows avec Visual Studio
    add_compile_options(/W4 /WX-)
    add_definitions(-D_CRT_SECURE_NO_WARNINGS)
else()
    # Linux/macOS avec GCC/Clang
    add_compile_options(-Wall -Wextra -Wpedantic)
endif()

# Trouver SFML
find_package(SFML 2.5 COMPONENTS system window graphics audio network REQUIRED)

# Trouver Threads
find_package(Threads REQUIRED)

# ============================================
# Sources du client
# ============================================
set(CLIENT_SOURCES
    src/main.cpp
    src/Background.cpp
    src/Button.cpp
    src/GameClient.cpp
    src/GameRenderer.cpp
    src/IconButton.cpp
    src/Lobby.cpp
    src/Logo.cpp
    src/Menu.cpp
    src/ModeSelection.cpp
    src/MultiplayerHUD.cpp
    src/NetworkClient.cpp
    src/WindowManager.cpp
    src/game/Bullet.cpp
    src/game/Enemy.cpp
    src/game/Game.cpp
    src/game/Player.cpp
    src/game/PowerUp.cpp
)

# Headers
set(CLIENT_HEADERS
    src/Background.hpp
    src/GameClient.hpp
    src/GameRenderer.hpp
    src/InputSystem.hpp
    src/Lobby.hpp
    src/Logo.hpp
    src/Menu.hpp
    src/ModeSelection.hpp
    src/MultiplayerHUD.hpp
    src/NetworkClient.hpp
    src/TestGame.hpp
    src/TextureManager.hpp
    src/WindowManager.hpp
    src/game/Bullet.hpp
    src/game/Enemy.hpp
    src/game/Game.hpp
    src/game/Player.hpp
    src/game/PowerUp.hpp
    src/game/RenderSystem.hpp
)

# ============================================
# Créer l'exécutable
# ============================================
add_executable(rtype_client ${CLIENT_SOURCES} ${CLIENT_HEADERS})

# Répertoires d'inclusion
target_include_directories(rtype_client PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/src
    ${CMAKE_CURRENT_SOURCE_DIR}/../Network
    ${CMAKE_CURRENT_SOURCE_DIR}/../rtype_engine
)

# Lier les bibliothèques
target_link_libraries(rtype_client PRIVATE
    sfml-system
    sfml-window
    sfml-graphics
    sfml-audio
    sfml-network
    Threads::Threads
)

# ============================================
# Copier les assets après la compilation
# ============================================
add_custom_command(TARGET rtype_client POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy_directory
    ${CMAKE_CURRENT_SOURCE_DIR}/assets
    $<TARGET_FILE_DIR:rtype_client>/assets
    COMMENT "Copying assets to build directory..."
)

# ============================================
# Installation (optionnel)
# ============================================
install(TARGETS rtype_client
    RUNTIME DESTINATION bin
)

install(DIRECTORY assets
    DESTINATION bin
)

# ============================================
# Configuration spécifique Windows
# ============================================
if(WIN32)
    # Copier les DLLs SFML sur Windows
    if(SFML_FOUND)
        # Trouver les DLLs SFML
        foreach(SFML_MODULE system window graphics audio network)
            if(EXISTS "${SFML_INCLUDE_DIR}/../bin/sfml-${SFML_MODULE}-2.dll")
                add_custom_command(TARGET rtype_client POST_BUILD
                    COMMAND ${CMAKE_COMMAND} -E copy_if_different
                    "${SFML_INCLUDE_DIR}/../bin/sfml-${SFML_MODULE}-2.dll"
                    $<TARGET_FILE_DIR:rtype_client>
                    COMMENT "Copying SFML DLL: sfml-${SFML_MODULE}-2.dll"
                )
            endif()
        endforeach()
    endif()
    
    # Définir l'icône et les propriétés Windows (optionnel)
    set_target_properties(rtype_client PROPERTIES
        WIN32_EXECUTABLE ON  # Pas de console sur Windows
    )
endif()

# ============================================
# Messages d'information
# ============================================
message(STATUS "===========================================")
message(STATUS "R-Type Client Configuration")
message(STATUS "===========================================")
message(STATUS "Build type: ${CMAKE_BUILD_TYPE}")
message(STATUS "C++ Standard: ${CMAKE_CXX_STANDARD}")
message(STATUS "Compiler: ${CMAKE_CXX_COMPILER_ID}")
message(STATUS "SFML Version: ${SFML_VERSION}")
message(STATUS "Platform: ${CMAKE_SYSTEM_NAME}")
message(STATUS "===========================================")