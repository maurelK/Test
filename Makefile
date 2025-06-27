##
## EPITECH PROJECT, 2025
## Makefile settingup
## File description:
## my network game
##

SRC	=	SERVER/socket.c \
		SERVER/ressources.c \
		SERVER/player_management.c \
		SERVER/handle_cmd.c \
		SERVER/fork_eggs.c \
		SERVER/Commandes/connect.c \
		SERVER/Commandes/eject.c \
		SERVER/Commandes/forward.c \
		SERVER/Commandes/inventory.c \
		SERVER/Commandes/left.c \
		SERVER/Commandes/right.c \
		SERVER/Commandes/look.c \
		SERVER/main.c

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
