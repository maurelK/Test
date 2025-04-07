##
## EPITECH PROJECT, 2023
## my_Makefile
## File description:
## compilation
##

NAME	= Graphicals

SRCS	= arcade_ncurses.cpp

OBJS	= $(SRCS:.cpp=.o)  # Correcting this to handle .cpp files

all: $(NAME)

$(NAME): $(OBJS)
	g++ -shared -o ../lib/arcade_ncurses.so -fPIC $(SRCS) -lncurses  # Use OBJS instead of SRCS

clean:
	rm -rf *.o

fclean: clean
	rm -f $(NAME)

re: fclean all


