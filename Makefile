##
## EPITECH PROJECT, 2025
## Makefile
## File description:
## Makefile
##

NAME = imageCompressor

STACK = stack

BUILD_DIR = $(shell $(STACK) path --local-install-root)

EXEC = $(BUILD_DIR)/bin/$(NAME)

TARGET = ./$(NAME)

.PHONY: all build clean fclean re run

all: build

build:
	$(STACK) build
	cp $(EXEC) $(TARGET)

clean:
	$(STACK) clean

fclean: clean
	rm -f $(TARGET)

re: fclean all

run: build
	./$(TARGET)
