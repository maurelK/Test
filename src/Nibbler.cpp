#include "../include/Nibbler.hpp"
#include <algorithm>
#include <random>
#include <cmath>

Nibbler::Nibbler() : currentDir(RIGHT), nextDir(RIGHT), score(0), gameOver(false)
{
    init();
}

void Nibbler::init()
{
    body.clear();
    foods.clear();
    walls.clear();

    body.push_back({gridWidth / 2, gridHeight - 2});
    body.push_back({gridWidth / 2, gridHeight - 1});
    body.push_back({gridWidth / 2, gridHeight});

    currentDir = nextDir = RIGHT;
    score = 0;
    gameOver = false;
    moveTimer = 0;

    generateMaze();
    spawnFood();
    updateRenderData();
}

void Nibbler::generateMaze()
{
    walls.push_back({0, 0, gridWidth, 1});
    walls.push_back({0, gridHeight - 1, gridWidth, 1});
    walls.push_back({0, 0, 1, gridHeight});
    walls.push_back({gridWidth - 1, 0, 1, gridHeight});

    walls.push_back({5, 5, 10, 1});
    walls.push_back({15, 5, 1, 10});
    walls.push_back({10, 15, 10, 1});
    walls.push_back({5, 10, 1, 5});
}

void Nibbler::spawnFood(int count)
{
    static std::random_device rd;
    static std::mt19937 gen(rd());
    std::uniform_int_distribution<int> distX(1, gridWidth - 2);
    std::uniform_int_distribution<int> distY(1, gridHeight - 2);

    for (int i = 0; i < count; ++i)
    {
        std::pair<int, int> newFood;
        do
        {
            newFood = {distX(gen), distY(gen)};
        } while (!isPositionFree(newFood.first, newFood.second));
        foods.push_back(newFood);
    }
}

bool Nibbler::isPositionFree(int x, int y) const
{
    for (const auto &segment : body)
    {
        if (segment.first == x && segment.second == y)
            return false;
    }

    for (const auto &wall : walls)
    {
        if (x >= wall.x && x < wall.x + wall.width &&
            y >= wall.y && y < wall.y + wall.height)
            return false;
    }

    for (const auto &food : foods)
    {
        if (food.first == x && food.second == y)
            return false;
    }

    return true;
}

void Nibbler::update()
{
    if (gameOver)
        return;

    moveTimer += 0.036f;
    if (moveTimer >= moveInterval)
    {
        moveTimer = 0;
        currentDir = nextDir;
        moveSnake();
        handleWallCollision();
        if (checkCollision())
        {
            gameOver = true;
        }
        checkFood();
        updateRenderData();
    }
}

void Nibbler::handleWallCollision()
{
    auto &head = body.front();

    for (const auto &wall : walls)
    {
        if (head.first >= wall.x && head.first < wall.x + wall.width &&
            head.second >= wall.y && head.second < wall.y + wall.height)
        {
            if (currentDir == UP || currentDir == DOWN)
            {
                if (isPositionFree(head.first + 1, head.second))
                {
                    nextDir = RIGHT;
                }
                else if (isPositionFree(head.first - 1, head.second))
                {
                    nextDir = LEFT;
                }
                else
                {
                    gameOver = true;
                }
            }
            else
            {
                if (isPositionFree(head.first, head.second + 1))
                {
                    nextDir = DOWN;
                }
                else if (isPositionFree(head.first, head.second - 1))
                {
                    nextDir = UP;
                }
                else
                {
                    gameOver = true;
                }
            }
            moveSnake();
            return;
        }
    }
}

void Nibbler::moveSnake()
{
    auto newHead = body.front();

    switch (currentDir)
    {
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

bool Nibbler::checkCollision() const
{
    const auto &head = body.front();

    for (size_t i = 1; i < body.size(); ++i)
    {
        if (head == body[i])
        {
            return true;
        }
    }

    return false;
}

void Nibbler::checkFood()
{
    auto &head = body.front();
    auto it = std::find(foods.begin(), foods.end(), head);

    if (it != foods.end())
    {
        foods.erase(it);
        score += 10;

        body.push_back(body.back());

        if (foods.empty())
        {
            spawnFood(5);
        }
    }
}

bool Nibbler::handleInput(int key)
{
    switch (key)
    {
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

void Nibbler::updateRenderData()
{
    renderData.entities.clear();
    renderData.texts.clear();
    renderData.shouldClose = false;

    for (int x = 0; x < gridWidth; x++)
    {
        renderData.entities.push_back({x, 0, '#', 4});
        renderData.entities.push_back({x, gridHeight - 1, '#', 4});
    }
    for (int y = 1; y < gridHeight - 1; y++)
    {
        renderData.entities.push_back({0, y, '#', 4});
        renderData.entities.push_back({gridWidth - 1, y, '#', 4});
    }
    for (const auto &wall : walls)
    {
        for (int y = wall.y; y < wall.y + wall.height; ++y)
        {
            for (int x = wall.x; x < wall.x + wall.width; ++x)
            {
                renderData.entities.push_back({x, y, '#', 4});
            }
        }
    }
    for (size_t i = 0; i < body.size(); ++i)
    {
        renderData.entities.push_back({body[i].first,
                                       body[i].second,
                                       (i == 0) ? '@' : 'o',
                                       (i == 0) ? 1 : 2});
    }
    for (const auto &food : foods)
    {
        renderData.entities.push_back({food.first, food.second, '*', 3});
    }
    renderData.texts.push_back({gridWidth + 2, 1, "NIBBLER"});
    renderData.texts.push_back({gridWidth + 2, 2, "========="});
    renderData.texts.push_back({gridWidth + 2, 4, "Score: " + std::to_string(score)});
    renderData.texts.push_back({gridWidth + 2, 6, "Controls:"});
    renderData.texts.push_back({gridWidth + 2, 7, "Z - Haut"});
    renderData.texts.push_back({gridWidth + 2, 8, "S - Bas"});
    renderData.texts.push_back({gridWidth + 2, 9, "Q - Gauche"});
    renderData.texts.push_back({gridWidth + 2, 10, "D - Droite"});
    renderData.texts.push_back({gridWidth + 2, 11, "R - Restart"});

    if (gameOver)
    {
        renderData.texts.push_back({gridWidth / 2 - 10, gridHeight / 2, "GAME OVER - Press R to restart"});
    }
}

const RenderData &Nibbler::getRenderData() const
{
    return renderData;
}

extern "C"
{
    IGame *createGame()
    {
        return new Nibbler();
    }

    void deleteGame(IGame *game)
    {
        delete static_cast<Nibbler *>(game);
    }
}