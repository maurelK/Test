add_library(ecs_core 
    include/ECS_architecture/*.hpp
    src/ECS_architecture/EntityManager.cpp 
add_executable(rtype_client
    src/main_client.cpp
    ${ecs_core}
    # + systèmes client
)

add_executable(rtype_server  
    src/main_server.cpp
    ${ecs_core}
)