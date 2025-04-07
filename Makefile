##
## EPITECH PROJECT, 2023
## my_Makefile
## File description:
## compilation
##

NAME	= arcade

core	=	Core

games	=	Games

graphicals	=	Graphics

SRCS	= Acore.cpp	

OBJS	= $(SRCS:.c=.o)

all: $(NAME)

$(NAME): $(OBJS)

	g++ $(SRCS) -o $(NAME) -fno-gnu-unique

clean:
	rm -rf *.o

fclean: clean
	rm -f $(NAME)

re: fclean all


