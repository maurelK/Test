NAME = client_rtype
CXX = g++
CXXFLAGS = -Wall -Wextra -std=c++17 -Isrc -Isrc/game -I../Network -I../rtype_engine
LDFLAGS = $(shell pkg-config --libs sfml-graphics sfml-window sfml-system)

SRC = src/main.cpp \
      src/GameClient.cpp \
      src/Menu.cpp \
      src/IconButton.cpp\
      src/Button.cpp \
      src/Background.cpp \
      src/Logo.cpp \
      src/WindowManager.cpp \
      src/GameRenderer.cpp \
      src/ModeSelection.cpp \
      src/Lobby.cpp \
      src/NetworkClient.cpp \
      src/MultiplayerHUD.cpp \
      src/game/Game.cpp \
      src/game/PowerUp.cpp \
      src/game/Bullet.cpp \
      src/game/Enemy.cpp \
      src/game/Player.cpp \
      ../Network/LobbyManager.cpp \
      ../Network/Networkmanager.cpp \
      ../Network/tcp_server.cpp \
      ../Network/udp_server.cpp

OBJ = $(SRC:.cpp=.o)

# ======================================================
#   BUILD TARGETS
# ======================================================

all: $(NAME)

$(NAME): $(OBJ)
	$(CXX) $(CXXFLAGS) -o $(NAME) $(OBJ) $(LDFLAGS) -lboost_system -pthread

%.o: %.cpp
	$(CXX) $(CXXFLAGS) -c $< -o $@

clean:
	rm -f $(OBJ)

fclean: clean
	rm -f $(NAME) test_local test_game test_render server_rtype

re: fclean all

run: $(NAME)
	./$(NAME)

# ======================================================
#   SERVER BUILD
# ======================================================

server_rtype: ../Network/main_server.cpp \
               ../Network/Networkmanager.cpp \
               ../Network/LobbyManager.cpp \
               ../Network/tcp_server.cpp \
               ../Network/udp_server.cpp
	$(CXX) -Wall -Wextra -std=c++17 -I../Network -pthread -lboost_system \
	-o server_rtype $^

# ======================================================
#   TESTS (local et complet)
# ======================================================

test_local:
	$(CXX) $(CXXFLAGS) -o test_local main_test.cpp src/GameClient.cpp \
		src/game/PowerUp.cpp src/game/Bullet.cpp src/game/Enemy.cpp src/game/Player.cpp src/IconButton.cpp \
		src/GameRenderer.cpp src/Background.cpp src/WindowManager.cpp src/NetworkClient.cpp src/Button.cpp\
		$(LDFLAGS) -pthread -lboost_system

test_game:
	$(CXX) $(CXXFLAGS) -o test_game main_test_game.cpp \
		src/game/Game.cpp src/game/Player.cpp src/game/Enemy.cpp src/game/Bullet.cpp src/game/PowerUp.cpp \
		$(LDFLAGS) -pthread

.PHONY: all clean fclean re run test_local test_game server_rtype
