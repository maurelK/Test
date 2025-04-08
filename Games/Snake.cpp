#include "Snake.hpp"
#include <chrono>
#include <algorithm>

SnakeGame::SnakeGame() : _currentDir(RIGHT), _nextDir(RIGHT), _state(RUNNING), _score(0), _highScore(0), _elapsedTime(0.0f), _frameRate(0.0f), _moveInterval(0.2f), _timeSinceLastMove(0.0f), _wallCollision(false), _rng(std::random_device{}()), _distX(0, 30), _distY(0, 20)
{
    init();
}

void SnakeGame::init()
{
    this->_snake.clear();
    this->_snake.push_back({5, 10});
    this->_snake.push_back({4, 10});
    this->_snake.push_back({3, 10});

    this->_currentDir = RIGHT;
    this->_nextDir = RIGHT;
    this->_score = 0;
    this->_state = RUNNING;
    this->_timeSinceLastMove = 0.0f;
    spawnFood();
}

void SnakeGame::update()
{
    static auto lastTime = std::chrono::high_resolution_clock::now();
    auto now = std::chrono::high_resolution_clock::now();
    float deltaTime = std::chrono::duration<float>(now - lastTime).count();
    lastTime = now;

    this->_elapsedTime += deltaTime;
    this->_timeSinceLastMove += deltaTime;
    this->_frameRate = 1.0f / deltaTime;

    if (this->_state != RUNNING)
        return;
    if (this->_timeSinceLastMove >= this->_moveInterval) {
        this->_timeSinceLastMove = 0.0f;
        this->_currentDir = this->_nextDir;
        moveSnake();
        if (checkCollision()) {
            this->_state = SnakeGame::GAME_OVER;
            updateHighScore();
            return;
        }
        if (this->_snake.front() == this->_food) {
            growSnake();
            this->_score += 10;
            spawnFood();
        }
    }
}

void SnakeGame::spawnFood()
{
    Position newFood;

    do {
        newFood = {this->_distX(_rng), this->_distY(_rng)};
    } while (std::find(_snake.begin(), this->_snake.end(), newFood) != this->_snake.end());

    this->_food = newFood;
}

bool SnakeGame::checkCollision() const
{
    const Position &head = this->_snake.front();

    if (this->_wallCollision) {
        if (head.x < 0 || head.x >= 30 || head.y < 0 || head.y >= 20) {
            return true;
        }
    } else {

    }
    for (size_t i = 1; i < this->_snake.size(); ++i) {
        if (head == this->_snake[i]) {
            return true;
        }
    }
    return false;
}

void SnakeGame::moveSnake()
{
    Position newHead = this->_snake.front();

    switch (_currentDir) {
    case UP:
        newHead.y--;
        break;
    case DOWN:
        newHead.y++;
        break;
    case LEFT:
        newHead.x--;
        break;
    case RIGHT:
        newHead.x++;
        break;
    }
    if (!_wallCollision) {
        if (newHead.x < 0)
            newHead.x = 29;
        else if (newHead.x >= 30)
            newHead.x = 0;
        if (newHead.y < 0)
            newHead.y = 19;
        else if (newHead.y >= 20)
            newHead.y = 0;
    }
    this->_snake.insert(this->_snake.begin(), newHead);
    this->_snake.pop_back();
}

void SnakeGame::growSnake()
{
    this->_snake.push_back(this->_snake.back());
    this->_moveInterval = std::max(0.05f, this->_moveInterval * 0.95f);
}

void SnakeGame::resetGame()
{
    init();
    this->_state = SnakeGame::RUNNING;
}

void SnakeGame::updateHighScore()
{
    if (this->_score > this->_highScore) {
        this->_highScore = this->_score;
    }
}

bool SnakeGame::isGameOver() const {
    return _state == SnakeGame::GAME_OVER;
}

void SnakeGame::handleInput(int input) {
    switch (input) {
        case 0: // UP
            if (_currentDir != DOWN) _nextDir = UP;
            break;
        case 1: // DOWN
            if (_currentDir != UP) _nextDir = DOWN;
            break;
        case 2: // LEFT
            if (_currentDir != RIGHT) _nextDir = LEFT;
            break;
        case 3: // RIGHT
            if (_currentDir != LEFT) _nextDir = RIGHT;
            break;
        case 4: // RESET
            resetGame();
            break;
    }
}
const std::vector<std::string>& SnakeGame::getDisplay() const {
    static std::vector<std::string> display;
    display.clear();
    
    // Create empty grid
    for (int y = 0; y < 20; y++) {
        std::string row(30, ' ');
        display.push_back(row);
    }
    
    // Draw food
    display[_food.y][_food.x] = 'O';
    
    // Draw snake
    for (const auto& segment : _snake) {
        display[segment.y][segment.x] = segment == _snake.front() ? 'H' : '#';
    }
    
    return display;
}
extern "C" {
    __attribute__((visibility("default"))) IGame* createGameInstance() 
    {
        return new SnakeGame();
    }

    __attribute__((visibility("default"))) void deleteGameInstance(IGame* game) 
    {
        delete game;
    }
}
