CXX = g++
CXXFLAGS = -std=c++20 -I./includes
LDFLAGS = -lsfml-graphics -lsfml-window -lsfml-system -lsfml-network -lsfml-audio

GUI_SRCS = src/main.cpp src/Window.cpp src/Cinematic.cpp src/Loading.cpp src/Music.cpp src/View.cpp src/Background.cpp src/BoolMenu.cpp src/NetworkManager.cpp	src/Menu.cpp src/GUI.cpp
GUI_OBJS = $(GUI_SRCS:.cpp=.o)
GUI_EXEC = zappy_gui

SERVER_SRCS = src/main_server.cpp src/Server.cpp
SERVER_OBJS = $(SERVER_SRCS:.cpp=.o)
SERVER_EXEC = zappy_server

all: $(GUI_EXEC) $(SERVER_EXEC)

$(GUI_EXEC): $(GUI_OBJS)
	$(CXX) $(CXXFLAGS) -o $(GUI_EXEC) $(GUI_OBJS) $(LDFLAGS)

$(SERVER_EXEC): $(SERVER_OBJS)
	$(CXX) $(CXXFLAGS) -o $(SERVER_EXEC) $(SERVER_OBJS) $(LDFLAGS)

%.o: %.cpp
	$(CXX) $(CXXFLAGS) -c $< -o $@

clean:
	rm -f $(GUI_OBJS) $(SERVER_OBJS)

fclean: clean
	rm -f $(GUI_EXEC) $(SERVER_EXEC)

re: fclean all

.PHONY: all clean fclean re
