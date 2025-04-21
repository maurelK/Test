#include "../include/Snake.hpp"
#include <algorithm>
#include <random>

Snake::Snake() : currentDir(RIGHT), nextDir(RIGHT), score(0), highScore(0), gameOver(false), isPaused(false),
                 moveSpeed(0.1f), baseSpeed(0.1f), speedIncreaseThreshold(50)
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
    highScore = std::max(score, highScore);
    gameOver = false;

    spawnFood();
    updateRenderData();
}

void Snake::update()
{
    if (gameOver || isPaused)
        return;
    static float timeSinceLastMove = 0.0f;
    const float deltaTime = 0.01f;
    timeSinceLastMove += deltaTime;

    if (timeSinceLastMove >= moveSpeed) {
        timeSinceLastMove = 0.0f;
        currentDir = nextDir;
        moveSnake();
        if (checkCollision()) {
            gameOver = true;
        } else if (body.front() == food) {
            growSnake();
            score += 10;
            if (score > highScore) {
                highScore = score;
            }
            spawnFood();
        }
        updateRenderData();
    }
}

bool Snake::handleInput(int key)
{
    switch (key) {
    case 'z':
    case 'Z':
    case KEY_UP:
    if (currentDir != DOWN)
        nextDir = UP;
            return true;
    case 's':
    case 'S':
    case KEY_DOWN:
    if (currentDir != UP)
        nextDir = DOWN;
            return true;
    case 'q':
    case 'Q':
    case KEY_LEFT:
    if (currentDir != RIGHT)
        nextDir = LEFT;
            return true;
    case 'd':
    case 'D':
    case KEY_RIGHT:
    if (currentDir != LEFT)
        nextDir = RIGHT;
            return true;
    
    case 'r':
    case 'R':
        if (gameOver)
            init();
        return true;
    default:
        return false; // Input not handled by game
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
    // Spawn food within playable area (avoid borders)
    std::uniform_int_distribution<int> distX(1, gridWidth - 2);
    std::uniform_int_distribution<int> distY(1, gridHeight - 2);

    do
    {
        food = {distX(gen), distY(gen)};
    } while (std::find(body.begin(), body.end(), food) != body.end());
}

bool Snake::checkCollision() const
{
    const auto &head = body.front();

    // Check wall collisions (1 unit inside border)
    if (head.first <= 0 || head.first >= gridWidth - 1 ||
        head.second <= 0 || head.second >= gridHeight - 1)
    {
        return true;
    }
    // Check self collisions
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
    for (size_t i = 0; i < body.size(); ++i)
    {
        renderData.entities.push_back({body[i].first,
                                       body[i].second,
                                       (i == 0) ? '@' : 'o',
                                       (i == 0) ? 1 : 2});
    }

    renderData.entities.push_back({food.first, food.second, 'X', 3});

    renderData.texts.push_back({gridWidth + 2, 1, "SNAKE"});
    renderData.texts.push_back({gridWidth + 2, 2, "======"});
    renderData.texts.push_back({gridWidth + 2, 4, "Score: " + std::to_string(score)});
    renderData.texts.push_back({gridWidth + 2, 5, "High: " + std::to_string(highScore)});
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