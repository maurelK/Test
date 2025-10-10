NAME = client_rtype
CXX = g++
CXXFLAGS = -Wall -Wextra -std=c++17 -Isrc -Isrc/game -I../Network -I../rtype_engine
LDFLAGS = $(shell pkg-config --libs sfml-graphics sfml-window sfml-system)

SRC = src/main.cpp src/GameClient.cpp src/Menu.cpp src/Background.cpp src/Logo.cpp \
      src/WindowManager.cpp src/GameRenderer.cpp src/TestGame.cpp \
      src/game/PowerUp.cpp src/NetworkClient.cpp
OBJ = $(SRC:.cpp=.o)

all: $(NAME)

$(NAME): $(OBJ)
	$(CXX) $(CXXFLAGS) -o $(NAME) $(OBJ) $(LDFLAGS)

%.o: %.cpp
	$(CXX) $(CXXFLAGS) -c $< -o $@

test_render:
	$(CXX) $(CXXFLAGS) -o test_render src/test_render.cpp $(LDFLAGS)

test_movement:
	$(CXX) -Wall -Wextra -std=c++17 -Isrc -Isrc/game -I../Network -I../rtype_engine \
	src/test_movement.cpp -o test_movement \
	-lsfml-graphics -lsfml-window -lsfml-system

test_input:
	$(CXX) -Wall -Wextra -std=c++17 -Isrc -Isrc/game -I../Network -I../rtype_engine \
	src/test_input.cpp -o test_input \
	-lsfml-graphics -lsfml-window -lsfml-system

clean:
	rm -f $(OBJ)

fclean: clean
	rm -f $(NAME) test_render

re: fclean all

run: $(NAME)
	./$(NAME)

.PHONY: all clean fclean re run test_render
