##
## EPITECH PROJECT, 2023
## Arcade Makefile
## File description:
## Build system for arcade (core + graphicals + games)
##

NAME        = arcade
CXX         = g++
CXXFLAGS    = -fPIC -fno-gnu-unique
LDFLAGS     = -ldl
SHARED      = -shared

SRC_DIR     = src
INC_DIR     = include
LIB_DIR     = lib
BIN_DIR     = .

CORE_SRCS   = $(SRC_DIR)/Acore.cpp $(SRC_DIR)/main.cpp
CORE_OBJS   = $(CORE_SRCS:.cpp=.o)
CORE_TARGET = $(BIN_DIR)/$(NAME)

GRAPHICALS  = ncurses 
NCURSES_LIB = $(LIB_DIR)/arcade_ncurses.so
#SDL_LIB     = $(LIB_DIR)/arcade_sdl.so

GAMES       = snake nibbler
SNAKE_LIB   = $(LIB_DIR)/arcade_snake.so
NIBBLER_LIB = $(LIB_DIR)/arcade_nibbler.so

all: core graphicals games

core: $(CORE_TARGET)

$(CORE_TARGET): $(CORE_OBJS)
	@mkdir -p $(LIB_DIR)
	$(CXX) $(CXXFLAGS) $(CORE_OBJS) -o $(CORE_TARGET) $(LDFLAGS)
	@echo "✓ Core compiled"

graphicals: $(GRAPHICALS)

ncurses:
	$(CXX) $(CXXFLAGS) $(SHARED) $(SRC_DIR)/arcade_ncurses.cpp -o $(NCURSES_LIB) -lncurses -I$(INC_DIR)
	@echo "✓ Ncurses library compiled"

#sdl:
#	$(CXX) $(CXXFLAGS) $(SHARED) $(SRC_DIR)/arcade_sdl.cpp -o $(SDL_LIB) -lSDL2 -I$(INC_DIR)
#	@echo "✓ SDL library compiled"

games: $(GAMES)

snake:
	$(CXX) $(CXXFLAGS) $(SHARED) $(SRC_DIR)/Snake.cpp -o $(SNAKE_LIB) -I$(INC_DIR)
	@echo "✓ Snake game compiled"

nibbler:
	$(CXX) $(CXXFLAGS) $(SHARED) $(SRC_DIR)/Nibbler.cpp -o $(NIBBLER_LIB) -I$(INC_DIR)
	@echo "✓ Nibbler game compiled"

clean:
	rm -f $(CORE_OBJS)
	@echo "✓ Object files removed"

fclean: clean
	rm -f $(CORE_TARGET)
	rm -f $(LIB_DIR)/*.so
	@echo "✓ Binaries and libraries removed"

re: fclean all
