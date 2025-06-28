##
## EPITECH PROJECT, 2025
## Makefile settingup
## File description:
## my network game
##

SRC	=	socket.c \
		ressources.c \
		player_management.c \
		handle_cmd.c \
		fork_eggs.c \
		Commandes/connect.c \
		Commandes/eject.c \
		Commandes/forward.c \
		Commandes/inventory.c \
		Commandes/left.c \
		Commandes/right.c \
		Commandes/look.c \
		main.c

OBJ	=	$(SRC:.c=.o)

NAME	=	zappy_server

all:	$(NAME)

$(NAME):	$(OBJ)
	gcc -o $(NAME) $(SRC)

clean:
	rm -f   $(OBJ)

fclean:	clean
	rm -f   $(NAME)

re:	fclean  all
