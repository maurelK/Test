##
## EPITECH PROJECT, 2023
## my_Makefile
## File description:
## compilation
##

NAME1   = ../lib/arcade_ncurses.so
NAME2   = ../lib/arcade_sdl2.so

SRCS1   = arcade_ncurses.cpp
SRCS2   = arcade_sdl2.cpp

OBJS1   = $(SRCS1:.cpp=.o)
OBJS2   = $(SRCS2:.cpp=.o)

CXX     = g++
CXXFLAGS = -Wall -Wextra -Werror -std=c++20 -fPIC -I.

all: $(NAME1) $(NAME2)

$(NAME1): $(SRCS1)
	$(CXX) $(CXXFLAGS) -shared -o $@ $^ -lncurses

$(NAME2): $(SRCS2)
	$(CXX) $(CXXFLAGS) -shared -o $@ $^ $(shell sdl2-config --cflags --libs) -lSDL2_ttf

clean:
	rm -rf *.o

fclean: clean
	rm -f $(NAME1) $(NAME2)

re: fclean all
