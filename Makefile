##
## EPITECH PROJECT, 2023
## Makefile settingup
## File description:
## Essai de find du biggest square
##

SRC	=	$(wildcard *.c)

OBJ	=	$(SRC:.c=.o)

NAME	=	myftp

all:	$(NAME)

$(NAME):	$(OBJ)
	gcc -o $(NAME) $(SRC)

clean:
	rm -f   $(OBJ)

fclean:	clean
	rm -f   $(NAME)

re:	fclean  all
