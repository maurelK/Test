##
## EPITECH PROJECT, 2023
## my_Makefile
## File description:
## compilation
##

NAME	= Core

SRCS	= Acore.cpp	

OBJS	= $(SRCS:.c=.o)

all: $(NAME)

$(NAME): $(OBJS)

	g++ $(SRCS) -o $(NAME)

clean:
	rm -rf *.o

fclean: clean
	rm -f $(NAME)

re: fclean all


