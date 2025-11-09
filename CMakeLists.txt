cmake_minimum_required(VERSION 3.15)
project(RType VERSION 1.0.0 LANGUAGES CXX)

# -------------------------------------------
# Standard C++20
# -------------------------------------------
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)
add_compile_options(-include utility)

# Options de compilation selon la plateforme
if(MSVC)
    add_compile_options(/W4 /WX-)
    add_definitions(-D_CRT_SECURE_NO_WARNINGS)
else()
    add_compile_options(-Wall -Wextra -Wpedantic)
endif()

# -------------------------------------------
# Dépendances communes
# -------------------------------------------
find_package(Threads REQUIRED)

# Inclure le moteur
add_subdirectory(engine)

# -------------------------------------------
# Serveur
# -------------------------------------------
if(NOT ONLY_CLIENT)
    find_package(Boost REQUIRED COMPONENTS system)

    set(SERVER_SOURCES
        main.cpp
        Network/LobbyManager.cpp
        Network/Networkmanager.cpp
        Network/tcp_server.cpp
        Network/udp_server.cpp
        system/CollisionSystem.cpp
        system/EnemySystem.cpp
        system/GameServer.cpp
        system/HealthSystem.cpp
        system/MovementSystem.cpp
        system/ProjectileSystem.cpp
        system/SpawnSystem.cpp
    )

    add_executable(rtype_server ${SERVER_SOURCES})

    target_include_directories(rtype_server PRIVATE
        ${CMAKE_CURRENT_SOURCE_DIR}
        ${Boost_INCLUDE_DIRS}
    )

    target_link_libraries(rtype_server PRIVATE
        Threads::Threads
        ${Boost_LIBRARIES}
        engine  # lien vers le moteur
    )

    install(TARGETS rtype_server RUNTIME DESTINATION bin)
endif()

# -------------------------------------------
# Client
# -------------------------------------------
find_package(SFML 2.5 COMPONENTS system window graphics audio network REQUIRED)

set(CLIENT_SOURCES
    Client1/src/main.cpp
    Client1/src/Background.cpp
    Client1/src/Button.cpp
    Client1/src/GameClient.cpp
    Client1/src/GameRenderer.cpp
    Client1/src/IconButton.cpp
    Client1/src/Logo.cpp
    Client1/src/Menu.cpp
    Client1/src/ModeSelection.cpp
    Client1/src/MultiplayerHUD.cpp
    Client1/src/NetworkClient.cpp
    Client1/src/WindowManager.cpp
    Client1/src/game/Bullet.cpp
    Client1/src/game/Enemy.cpp
    Client1/src/game/Game.cpp
    Client1/src/game/Player.cpp
    Client1/src/game/PowerUp.cpp
    Client1/src/ConnectScreen.cpp
    Client1/src/LobbyListScreen.cpp
)

add_executable(rtype_client ${CLIENT_SOURCES})

target_include_directories(rtype_client PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}
    ${CMAKE_CURRENT_SOURCE_DIR}/Client1/src
)

target_link_libraries(rtype_client PRIVATE
    sfml-system
    sfml-window
    sfml-graphics
    sfml-audio
    sfml-network
    Threads::Threads
)

# Copier les assets après compilation
add_custom_command(TARGET rtype_client POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy_directory
    ${CMAKE_CURRENT_SOURCE_DIR}/Client1/assets
    $<TARGET_FILE_DIR:rtype_client>/assets
    COMMENT "Copying assets to build directory..."
)

# -------------------------------------------
# Installation
# -------------------------------------------
install(TARGETS rtype_client RUNTIME DESTINATION bin)
install(DIRECTORY Client1/assets DESTINATION bin)

# -------------------------------------------
# Messages de configuration
# -------------------------------------------
message(STATUS "===========================================")
message(STATUS "R-Type Project Configuration")
message(STATUS "===========================================")
message(STATUS "Build type: ${CMAKE_BUILD_TYPE}")
message(STATUS "C++ Standard: ${CMAKE_CXX_STANDARD}")
message(STATUS "Compiler: ${CMAKE_CXX_COMPILER_ID}")
message(STATUS "Platform: ${CMAKE_SYSTEM_NAME}")
if(SFML_FOUND)
    message(STATUS "SFML Version: ${SFML_VERSION}")
endif()
if(Boost_FOUND)
    message(STATUS "Boost Version: ${Boost_VERSION}")
endif()
if(ONLY_CLIENT)
    message(STATUS "Building: Client only")
else()
    message(STATUS "Building: Client + Server")
endif()
message(STATUS "===========================================")