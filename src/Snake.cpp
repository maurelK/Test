#include "../include/Snake.hpp"
#include <algorithm>
#include <random>

Snake::Snake() : currentDir(RIGHT), nextDir(RIGHT), score(0), gameOver(false)
{
    init();
}

void Snake::init()
{
    body.clear();
    body.push_back({gridWidth / 2, gridHeight / 2});
    body.push_back({gridWidth / 2 - 1, gridHeight / 2});
    body.push_back({gridWidth / 2 - 2, gridHeight / 2});

    currentDir = nextDir = RIGHT;
    score = 0;
    gameOver = false;

    spawnFood();
    updateRenderData();
}

void Snake::update()
{
    if (gameOver)
        return;

    static float timeSinceLastMove = 0.0f;
    const float deltaTime = 0.016f;
    timeSinceLastMove += deltaTime;

    if (timeSinceLastMove >= 0.2f) {
        timeSinceLastMove = 0.0f;
        currentDir = nextDir;
        moveSnake();
        if (checkCollision()) {
            gameOver = true;
        } else if (body.front() == food) {
            growSnake();
            score += 10;
            spawnFood();
        }
        updateRenderData();
    }
}

void Snake::handleInput(int key)
{
    switch (key) {
    case 'z':
    case 'Z':
    case KEY_UP:
        if (currentDir != DOWN)
            nextDir = UP;
        break;
    case 's':
    case 'S':
    case KEY_DOWN:
        if (currentDir != UP)
            nextDir = DOWN;
        break;
    case 'q':
    case 'Q':
    case KEY_LEFT:
        if (currentDir != RIGHT)
            nextDir = LEFT;
        break;
    case 'd':
    case 'D':
    case KEY_RIGHT:
        if (currentDir != LEFT)
            nextDir = RIGHT;
        break;
    case 'r':
    case 'R':
        if (gameOver)
            init();
        break;
    }
}

const RenderData &Snake::getRenderData() const
{
    return renderData;
}

void Snake::spawnFood()
{
    static std::random_device rd;
    static std::mt19937 gen(rd());
    std::uniform_int_distribution<int> distX(0, gridWidth - 1);
    std::uniform_int_distribution<int> distY(0, gridHeight - 1);

    do
    {
        food = {distX(gen), distY(gen)};
    } while (std::find(body.begin(), body.end(), food) != body.end());
}

bool Snake::checkCollision() const
{
    const auto &head = body.front();

    if (head.first < 0 || head.first >= gridWidth ||
        head.second < 0 || head.second >= gridHeight)
    {
        return true;
    }

    // Collision avec le corps
    for (size_t i = 1; i < body.size(); ++i)
    {
        if (head == body[i])
        {
            return true;
        }
    }

    return false;
}

void Snake::moveSnake()
{
    auto newHead = body.front();

    switch (currentDir) {
    case UP:
        newHead.second--;
        break;
    case DOWN:
        newHead.second++;
        break;
    case LEFT:
        newHead.first--;
        break;
    case RIGHT:
        newHead.first++;
        break;
    }
    body.insert(body.begin(), newHead);
    body.pop_back();
}

void Snake::growSnake()
{
    body.push_back(body.back());
}

void Snake::updateRenderData()
{
    renderData.entities.clear();
    renderData.texts.clear();

    for (size_t i = 0; i < body.size(); ++i) {
        renderData.entities.push_back({
            body[i].first,
            body[i].second,
            (i == 0) ? 'H' : 'o',
            (i == 0) ? 1 : 2
        });
    }

    renderData.entities.push_back({food.first, food.second, 'X', 3});
    renderData.texts.push_back({0, 0, "Score: " + std::to_string(score)});
    if (gameOver)
    {
        renderData.texts.push_back({10, 10, "GAME OVER - Press R to restart"});
    }
}

extern "C"
{
    IGame *createGame()
    {
        return new Snake();
    }

    void deleteGame(IGame *game)
    {
        delete static_cast<Snake *>(game);
    }
}