##
## EPITECH PROJECT, 2025
## my_Makefile
## File description:
## compilation
##

NAME	= raytracer

SRCS	= $(wildcard *.cpp)

OBJS	= $(SRCS:.cpp=.o)

CXX	= g++
CXXFLAGS = -Wall -Wextra -std=c++17
LIBS	= -lconfig++ -lsfml-graphics -lsfml-window -lsfml-system

all: $(NAME)

$(NAME): $(OBJS)
	$(CXX) $(CXXFLAGS) $(OBJS) -o $(NAME) $(LIBS)

clean:
	rm -f *.o

fclean: clean
	rm -f $(NAME)

re: fclean all