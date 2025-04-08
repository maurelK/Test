##
## EPITECH PROJECT, 2025
## my_Makefile
## File description:
## compilation
##

NAME = arcade

CXX = g++
CXXFLAGS = -Wall -Wextra -fPIC -fno-gnu-unique
LDFLAGS = -ldl

SRC_CORE = Core/Acore.cpp
OBJ_CORE = $(SRC_CORE:.cpp=.o)

INC = -I./Core -I./Graphics -I./Games

all: core games graphicals

core: $(OBJ_CORE)
	$(CXX) $(CXXFLAGS) -o $(NAME) $(OBJ_CORE) $(LDFLAGS)

games:
	$(MAKE) -C Games

graphicals:
	$(MAKE) -C Graphics

clean:
	rm -f $(OBJ_CORE)
	$(MAKE) -C Games clean
	$(MAKE) -C Graphics clean

fclean: clean
	rm -f $(NAME)
	$(MAKE) -C Games fclean
	$(MAKE) -C Graphics fclean

re: fclean all

.PHONY: all core games graphicals clean fclean re


