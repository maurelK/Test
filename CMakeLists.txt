cmake_minimum_required(VERSION 3.20)
project(rtype_server LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

include(${CMAKE_BINARY_DIR}/conan_toolchain.cmake OPTIONAL)

include_directories(
    ${CMAKE_SOURCE_DIR}/system
    ${CMAKE_SOURCE_DIR}/rtype_engine
    ${CMAKE_SOURCE_DIR}/Network
)

set(SERVER_SOURCES
    main_server.cpp
    system/GameServer.cpp
    system/MovementSystem.cpp
    system/CollisionSystem.cpp
    system/HealthSystem.cpp
    system/SpawnSystem.cpp
    system/ProjectileSystem.cpp
    system/EnemySystem.cpp
    Network/Networkmanager.cpp
    Network/tcp_server.cpp
    Network/udp_server.cpp
    Network/LobbyManager.cpp
)

set(SERVER_HEADERS
    system/GameServer.hpp
    system/MovementSystem.hpp
    system/CollisionSystem.hpp
    system/HealthSystem.hpp
    system/SpawnSystem.hpp
    system/ProjectileSystem.hpp
    system/EnemySystem.hpp
    rtype_engine/EntityManager.hpp
    rtype_engine/SystemManager.hpp
    rtype_engine/System.hpp
    rtype_engine/Components.hpp
    Network/Networkmanager.hpp
    Network/tcp_server.hpp
    Network/udp_server.hpp
    Network/LobbyManager.hpp
    Network/protocol.hpp
    Network/concurent_queue.hpp
)

add_executable(server ${SERVER_SOURCES} ${SERVER_HEADERS})

find_package(asio REQUIRED)
target_link_libraries(server PRIVATE asio::asio)

target_compile_definitions(server PRIVATE ASIO_STANDALONE RTYPE_SERVER)
target_include_directories(server PRIVATE ${CMAKE_SOURCE_DIR})

message(STATUS "✅ R-Type Server build configured successfully.")
