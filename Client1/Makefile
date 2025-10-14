NAME = client_rtype
CXX = g++
CXXFLAGS = -Wall -Wextra -std=c++17 -Isrc -Isrc/game -I../Network -I../rtype_engine
LDFLAGS = $(shell pkg-config --libs sfml-graphics sfml-window sfml-system)

SRC = src/main.cpp \
      src/GameClient.cpp \
      src/Menu.cpp \
      src/Background.cpp \
      src/Logo.cpp \
      src/WindowManager.cpp \
      src/GameRenderer.cpp \
      src/TestGame.cpp \
      src/ModeSelection.cpp \
      src/Lobby.cpp \
      src/game/PowerUp.cpp \
      src/NetworkClient.cpp \
      ../Network/LobbyManager.cpp \
      ../Network/Networkmanager.cpp \
      ../Network/tcp_server.cpp \
      ../Network/udp_server.cpp



OBJ = $(SRC:.cpp=.o)

all: $(NAME)

$(NAME): $(OBJ)
	$(CXX) $(CXXFLAGS) -o $(NAME) $(OBJ) $(LDFLAGS) -lboost_system -pthread

%.o: %.cpp
	$(CXX) $(CXXFLAGS) -c $< -o $@

clean:
	rm -f $(OBJ)

fclean: clean
	rm -f $(NAME) test_render

re: fclean all

run: $(NAME)
	./$(NAME)

server_rtype: ../Network/main_server.cpp \
               ../Network/Networkmanager.cpp \
               ../Network/LobbyManager.cpp \
               ../Network/tcp_server.cpp \
               ../Network/udp_server.cpp 
	$(CXX) -Wall -Wextra -std=c++17 -I../Network -pthread -lboost_system \
	-o server_rtype $^


.PHONY: all clean fclean re run test_render
