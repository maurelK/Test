##
## EPITECH PROJECT, 2025
## Main Makefile
## File description:
## compilation
##

all: core games graphicals

core:
	$(MAKE) -C Core

games:
	$(MAKE) -C Games

graphicals:
	$(MAKE) -C Graphics

clean:
	$(MAKE) -C Core clean
	$(MAKE) -C Games clean
	$(MAKE) -C Graphics clean

fclean: clean
	$(MAKE) -C Core fclean
	$(MAKE) -C Games fclean
	$(MAKE) -C Graphics fclean

re: fclean all

.PHONY: all core games graphicals clean fclean re