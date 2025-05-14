##
## EPITECH PROJECT, 2023
## my_Makefile
## File description:
## compilation
##

NAME	= raytracer

SRCS	= $(wildcard *.cpp)

OBJS    = $(SRCS:.cpp=.o)  

all: $(NAME)

$(NAME): $(OBJS) $(MAIN)
	g++ $(OBJS) -o $(NAME) -lconfig++

clean:
	rm -rf *.o test_scene

fclean: clean
	rm -f $(NAME)

re: fclean all
