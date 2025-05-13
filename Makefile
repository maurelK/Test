##
## EPITECH PROJECT, 2025
## tyetyyt
## File description:
## rttytuyytr
##

NAME	=	raytracer

SRCS	= 	$(wildcard *.cpp)

OBJS	= 	$(SRCS:.c=.o)

all: 		$(NAME)

$(NAME):	$(OBJS)

	g++ $(SRCS) -o $(NAME) -lconfig++
clean:

fclean: clean
	rm -rf *.a

re: fclean all
