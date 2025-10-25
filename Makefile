##
## EPITECH PROJECT, 2024
## my_pgp
## File description:
## Makefile
##

NAME	=	my_pgp

SRC	=	src/main.cpp	\
		src/args_parser.cpp	\
		src/error_handler.cpp	\
		src/utils.cpp	\
		src/xor_cipher.cpp	\
		src/rsa_cipher.cpp	\
		src/pgp_cipher.cpp	\
		src/aes_cipher.cpp

OBJ	=	$(SRC:.cpp=.o)

CXX	=	g++

CXXFLAGS	=	-Wall -Wextra -I./include -std=c++17

LDFLAGS	=	-lgmp -lgmpxx

all:	$(NAME)

$(NAME):	$(OBJ)
	$(CXX) -o $(NAME) $(OBJ) $(LDFLAGS)

%.o: %.cpp
	$(CXX) $(CXXFLAGS) -c $< -o $@

clean:
	rm -f $(OBJ)

fclean:	clean
	rm -f $(NAME)

re:	fclean all

.PHONY:	all clean fclean re
