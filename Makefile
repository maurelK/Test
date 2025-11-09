##
## R-TYPE SERVER MAKEFILE
##

NAME = GameServer

# Compilateur et flags
CXX = g++
CXXFLAGS = -std=c++20 -Wall -Wextra -Werror -pthread

# Dossiers
SRC_DIRS = system \
           engine/src/Core \
           engine/src/ECS_architecture \
           Network

INC_DIRS = -Iengine/include \
           -Isystem \
           -INetwork

# Fichiers sources
SRCS = $(wildcard $(addsuffix /*.cpp, $(SRC_DIRS))) main.cpp
OBJS = $(SRCS:.cpp=.o)

# Règles
all: $(NAME)

$(NAME): $(OBJS)
	@echo "🔧 Linking $(NAME)..."
	$(CXX) $(OBJS) $(CXXFLAGS) $(INC_DIRS) -o $(NAME)
	@echo "✅ Build complete: ./$(NAME)"

%.o: %.cpp
	@echo "🧩 Compiling $<"
	$(CXX) $(CXXFLAGS) $(INC_DIRS) -c $< -o $@

clean:
	@echo "🧹 Cleaning object files..."
	rm -f $(OBJS)

fclean: clean
	@echo "🧼 Removing binary..."
	rm -f $(NAME)

re: fclean all

.PHONY: all clean fclean re
