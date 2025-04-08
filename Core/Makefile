##
## EPITECH PROJECT, 2023
## my_Makefile
## File description:
## compilation
##


NAME = arcade

SRC = Acore.cpp
OBJ = $(SRC:.cpp=.o)

CXX = g++
CXXFLAGS = -Wall -Wextra -Werror -std=c++20 -fPIC -I../Graphics -I../Games -I.
LDFLAGS = -ldl

all: $(NAME)

$(NAME): $(OBJ)
	$(CXX) -o $(NAME) $(OBJ) $(LDFLAGS)

clean:
	rm -f $(OBJ)

fclean: clean
	rm -f $(NAME)

re: fclean all

.PHONY: all clean fclean re