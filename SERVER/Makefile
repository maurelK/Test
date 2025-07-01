##
## EPITECH PROJECT, 2025
## ze
## File description:
## zr
##

CC = gcc
SRC = fork_eggs.c handle_cmd.c handle_poll.c main.c main2.c main3.c \
      player_management.c ressources.c socket.c \
      Commandes/broad.c Commandes/connect.c Commandes/eject.c \
      Commandes/forward.c Commandes/incantation_cmd.c \
      Commandes/inventory.c Commandes/left.c Commandes/look.c Commandes/right.c
OBJ = $(SRC:.c=.o)
NAME = zappy_server
CFLAGS = -Wall -Wextra -Iinclude -lm

all: $(NAME)

$(NAME): $(OBJ)
	$(CC) $(OBJ) -o $(NAME) $(CFLAGS)

clean:
	rm -f $(OBJ)

fclean: clean
	rm -f $(NAME)

re: fclean all
