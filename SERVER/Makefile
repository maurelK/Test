##
## EPITECH PROJECT, 2025
## ze
## File description:
## zr
##

CC = gcc
SRC = $(wildcard *.c Commandes/*.c)
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
