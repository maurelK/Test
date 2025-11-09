#include "ConnectScreen.hpp"
#include <iostream>
#include <chrono>
#include <thread>
#include <cstdlib>

static sf::Color BG(5, 10, 25);
static sf::Color CYAN(0, 200, 255);
static sf::Color DARK(20, 30, 50);
static sf::Color YELLOW(255, 230, 60);
static sf::Color ORANGE(255, 107, 53);


TextField::TextField(const sf::Font& font, const std::string& labelText,
                     float centerX, float y, float width, float height)
    : text(""), isActive(false), isHovered(false), cursorTime(0.f)
{
    label.setFont(font);
    label.setString(labelText);
    label.setCharacterSize(20);
    label.setFillColor(CYAN);
    sf::FloatRect lb = label.getLocalBounds();
    label.setOrigin(lb.width / 2, 0);
    label.setPosition(centerX - 20, y - 35);

   box = RoundedRectangleShape({width, height}, 7.f);

    box.setOrigin(width / 2.f, 0);
    box.setPosition(centerX, y);
    box.setFillColor(DARK);
    box.setOutlineColor(CYAN);
    box.setOutlineThickness(2.f);

    content.setFont(font);
    content.setCharacterSize(24);
    content.setFillColor(sf::Color::White);
    content.setPosition(centerX - width / 2.f + 15, y + 10);
}


void TextField::update(const sf::Vector2f& mousePos, float deltaTime) {
    bool hoveredNow = box.getGlobalBounds().contains(mousePos);
    isHovered = hoveredNow;

    sf::Color currentOutline = box.getOutlineColor();
    sf::Color targetOutline;

    if (isActive)
        targetOutline = ORANGE;
    else if (isHovered)
        targetOutline = sf::Color(255, 170, 100);
    else
        targetOutline = CYAN;

    sf::Color blended(
        static_cast<sf::Uint8>(currentOutline.r + (targetOutline.r - currentOutline.r) * 0.08f),
        static_cast<sf::Uint8>(currentOutline.g + (targetOutline.g - currentOutline.g) * 0.08f),
        static_cast<sf::Uint8>(currentOutline.b + (targetOutline.b - currentOutline.b) * 0.08f)
    );
    box.setOutlineColor(blended);

    float targetThickness = isActive ? 4.f : (isHovered ? 3.f : 2.f);
    float currentThickness = box.getOutlineThickness();
    box.setOutlineThickness(currentThickness + (targetThickness - currentThickness) * 0.15f);

    float targetScale = isHovered ? 1.02f : 1.0f;
    box.setScale(targetScale, targetScale);
    label.setScale(targetScale, targetScale);
    content.setScale(targetScale, targetScale);

    if (isActive) {
        cursorTime += deltaTime;
        if (cursorTime > 1.0f) cursorTime = 0.f;

        std::string displayText = text;
        if (cursorTime < 0.5f) displayText += "|";
        content.setString(displayText);
    } else {
        content.setString(text);
    }
}

void TextField::draw(sf::RenderTarget& target) const {
    target.draw(label);
    target.draw(box);
    target.draw(content);
}

void TextField::handleInput(sf::Uint32 unicode) {
    if (!isActive || text.length() >= 30) return;
    if (unicode >= 32 && unicode < 127)
        text += static_cast<char>(unicode);
}

void TextField::handleBackspace() {
    if (isActive && !text.empty()) text.pop_back();
}

void TextField::setActive(bool active) {
    isActive = active;
    cursorTime = 0.f;
}

bool TextField::contains(const sf::Vector2f& point) const {
    return box.getGlobalBounds().contains(point);
}

ConnectScreenButton::ConnectScreenButton(const sf::Font& font, const std::string& label,
                                         float centerX, float y, float width, float height)
    : isHovered(false)
{
   rect = RoundedRectangleShape({width, height}, 7.f);

    rect.setOrigin(width / 2.f, 0);
    rect.setPosition(centerX, y);
    rect.setFillColor(sf::Color(0, 100, 150));
    rect.setOutlineColor(CYAN);
    rect.setOutlineThickness(2.f);

    text.setFont(font);
    text.setString(label);
    text.setCharacterSize(24);
    text.setFillColor(sf::Color::White);
    sf::FloatRect t = text.getLocalBounds();
    text.setOrigin(t.width / 2, t.height / 2);
    text.setPosition(centerX, y + height / 2);
}


void ConnectScreenButton::update(const sf::Vector2f& mousePos) {
    bool hoveredNow = rect.getGlobalBounds().contains(mousePos);

    sf::Color currentOutline = rect.getOutlineColor();
    sf::Color targetOutline = hoveredNow ? sf::Color(255, 140, 80) : CYAN;
    sf::Color blended(
        static_cast<sf::Uint8>(currentOutline.r + (targetOutline.r - currentOutline.r) * 0.15f),
        static_cast<sf::Uint8>(currentOutline.g + (targetOutline.g - currentOutline.g) * 0.15f),
        static_cast<sf::Uint8>(currentOutline.b + (targetOutline.b - currentOutline.b) * 0.15f)
    );
    rect.setOutlineColor(blended);
    if (hoveredNow) {
    rect.setOutlineThickness(4.f);
} else {
    rect.setOutlineThickness(2.f);
}


    float scale = hoveredNow ? 1.03f : 1.0f;
    rect.setScale(scale, scale);
    text.setScale(scale, scale);

    text.setFillColor(hoveredNow ? sf::Color(255, 210, 180) : sf::Color(230, 230, 230));

    isHovered = hoveredNow;
}

void ConnectScreenButton::draw(sf::RenderTarget& target) const {
    target.draw(rect);
    target.draw(text);
}

bool ConnectScreenButton::isClicked(const sf::Vector2f& mousePos) const {
    return rect.getGlobalBounds().contains(mousePos);
}


void ConnectScreen::initStars() {
    stars.clear();
    for (int i = 0; i < 100; ++i) {
        ConnectStar s;
        s.shape.setRadius(static_cast<float>(rand() % 2 + 1));
        s.shape.setFillColor(sf::Color(200, 200, 255, 180));
        s.shape.setPosition(static_cast<float>(rand() % window.getSize().x),
                            static_cast<float>(rand() % window.getSize().y));
        s.speed = static_cast<float>(rand() % 40 + 20);
        stars.push_back(s);
    }
}

void ConnectScreen::updateStars(float dt) {
    for (auto& s : stars) {
        sf::Vector2f pos = s.shape.getPosition();
        pos.y += s.speed * dt;
        if (pos.y > window.getSize().y)
            pos.y = 0.f;
        s.shape.setPosition(pos);
    }
}

void ConnectScreen::drawStars(sf::RenderTarget& target) const {
    for (const auto& s : stars)
        target.draw(s.shape);
}

ConnectScreen::ConnectScreen(sf::RenderWindow& win)
    : window(win),
    playerNameField(font, "NOM DU JOUEUR", win.getSize().x / 2.f, 150, 400, 50),
serverIPField(font, "ADRESSE IP DU SERVEUR", win.getSize().x / 2.f, 250, 400, 50),
serverPortField(font, "PORT", win.getSize().x / 2.f, 350, 400, 50),
connectButton(font, "SE CONNECTER", win.getSize().x / 2.f, 440, 400, 55),
backButton(font, "RETOUR MENU", win.getSize().x / 2.f, 525, 400, 55),


      state(ConnectState::Idle),
      activeField(nullptr)
{
    if (!font.loadFromFile("assets/OpenSans-Regular.ttf"))
        std::cerr << "Erreur : impossible de charger la police\n";

titleText.setFont(font);
titleText.setString("CONNEXION");
titleText.setCharacterSize(50);
titleText.setFillColor(sf::Color(255, 255, 255));
titleText.setStyle(sf::Text::Bold);

sf::FloatRect tb = titleText.getLocalBounds();
titleText.setOrigin(tb.width / 2, tb.height / 2);
titleText.setPosition(win.getSize().x / 2.f, 50);

    subtitleText.setFont(font);
    subtitleText.setCharacterSize(40);
    subtitleText.setFillColor(CYAN);
    sf::FloatRect sb = subtitleText.getLocalBounds();
    subtitleText.setOrigin(sb.width / 2, sb.height / 2);
    subtitleText.setPosition(win.getSize().x / 2.f, 90);

    statusText.setFont(font);
    statusText.setCharacterSize(18);
    statusText.setFillColor(sf::Color::Yellow);
    sf::FloatRect st = statusText.getLocalBounds();
    statusText.setOrigin(st.width / 2, st.height / 2);
    statusText.setPosition(win.getSize().x / 2.f, win.getSize().y - 40);

    playerNameField.text = "Player";
    serverIPField.text = "127.0.0.1";
    serverPortField.text = "4242";

    playerNameField.label.setPosition(
    playerNameField.label.getPosition().x - 50, 
    playerNameField.label.getPosition().y
);

serverIPField.label.setPosition(
    serverIPField.label.getPosition().x - 90, 
    serverIPField.label.getPosition().y
);

serverPortField.label.setPosition(
    serverPortField.label.getPosition().x -5,
    serverPortField.label.getPosition().y
);

    connectButton.text.setPosition(
connectButton.text.getPosition().x - 85,
connectButton.text.getPosition().y -15
);

    backButton.text.setPosition(
        backButton.text.getPosition().x -85,
        backButton.text.getPosition().y -15
);

    initStars();
}

ConnectScreen::~ConnectScreen() {
    running = false;
    if (networkThread.joinable())
        networkThread.join();
}

void ConnectScreen::update(const sf::Vector2f& mousePos, float deltaTime) {
    updateStars(deltaTime);
    playerNameField.update(mousePos, deltaTime);
    serverIPField.update(mousePos, deltaTime);
    serverPortField.update(mousePos, deltaTime);
    connectButton.update(mousePos);
    backButton.update(mousePos);
}

void ConnectScreen::draw(sf::RenderTarget& target) const {
    target.clear(BG);
    drawStars(target);
    
    sf::Text glow = titleText;
    glow.setFillColor(sf::Color(0, 200, 255, 80));
    glow.setScale(1.05f, 1.05f);
    target.draw(glow);

    target.draw(titleText);
    target.draw(subtitleText);
    playerNameField.draw(target);
    serverIPField.draw(target);
    serverPortField.draw(target);
    connectButton.draw(target);
    backButton.draw(target);
    target.draw(statusText);
}

void ConnectScreen::handleClick(const sf::Vector2f& mousePos) {
    playerNameField.setActive(false);
    serverIPField.setActive(false);
    serverPortField.setActive(false);
    activeField = nullptr;

    if (playerNameField.contains(mousePos)) {
        playerNameField.setActive(true);
        activeField = &playerNameField;
    } else if (serverIPField.contains(mousePos)) {
        serverIPField.setActive(true);
        activeField = &serverIPField;
    } else if (serverPortField.contains(mousePos)) {
        serverPortField.setActive(true);
        activeField = &serverPortField;
    }

    if (connectButton.isClicked(mousePos)) {
        setStatus("Connexion au serveur...", false);
        state = ConnectState::Connecting;
    } else if (backButton.isClicked(mousePos)) {
        state = ConnectState::Back;
    }
}

void ConnectScreen::handleTextInput(sf::Uint32 unicode) {
    if (activeField)
        activeField->handleInput(unicode);
}

void ConnectScreen::handleBackspace() {
    if (activeField)
        activeField->handleBackspace();
}

void ConnectScreen::setStatus(const std::string& status, bool isError) {
    statusText.setString(status);
    statusText.setFillColor(isError ? sf::Color::Red : sf::Color::Yellow);
}

void ConnectScreen::runNetwork(NetworkClient& client) {
    running = true;
    networkThread = std::thread([this, &client]() {
        try {
            if (!client.start()) {
                setStatus("Erreur démarrage réseau.", true);
                state = ConnectState::Error;
                return;
            }

            if (!client.sendLogin(getPlayerName())) {
                setStatus("Erreur login.", true);
                state = ConnectState::Error;
                return;
            }

            if (!client.joinLobby(1)) {
                setStatus("Erreur join lobby.", true);
                state = ConnectState::Error;
                return;
            }

            setStatus("Connecté. En attente d'autres joueurs...", false);
            state = ConnectState::WaitingStart;

            while (running) {
                auto evt = client.listenForServerEvents();
                if (evt.has_value() && evt.value() == PacketType::GAME_START) {
                    setStatus("Jeu en cours...", false);
                    state = ConnectState::GameStart;
                    break;
                }
                std::this_thread::sleep_for(std::chrono::milliseconds(100));
            }

        } catch (std::exception& e) {
            setStatus("Erreur réseau", true);
            state = ConnectState::Error;
        }
    });
}
