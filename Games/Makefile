# Games/Makefile

NAME = games
SRC = Snake.cpp
OBJ = $(SRC:.cpp=.o)
SO = ../lib/arcade_snake.so

CXX = g++
CXXFLAGS = -Wall -Wextra -Werror -std=c++20 -fPIC -I.
LDFLAGS = -shared

all: $(SO)

$(SO): $(SRC)
	$(CXX) $(CXXFLAGS) $^ -o $@ $(LDFLAGS)

clean:
	rm -f $(OBJ)

fclean: clean
	rm -f $(SO)

re: fclean all

.PHONY: all clean fclean re
