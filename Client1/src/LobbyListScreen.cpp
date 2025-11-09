#include "LobbyListScreen.hpp"
#include <iostream>
#include <cstdlib>
#include "RoundedRectangleShape.hpp"

static sf::Text makeText(const sf::Font& f, const std::string& s, unsigned sz, sf::Color c) {
    sf::Text t; t.setFont(f); t.setString(s); t.setCharacterSize(sz); t.setFillColor(c);
    return t;
}

static sf::Color BG_TOP(10, 10, 10);
static sf::Color BG_BOTTOM(0, 0, 0);


LobbyListScreen::LobbyListScreen(sf::RenderWindow& win)
: window(win)
{
    if (!font.loadFromFile("assets/OpenSans-Regular.ttf"))
        std::cerr << "[LobbyList] Erreur: police introuvable (assets/OpenSans-Regular.ttf)\n";

    lobbies = {
        {"RoomPavel",        2, 4},
        {"BenayaRoom",       1, 4},
        {"Maurel_Test",      3, 4},
        {"Public_Lobby_01",  0, 4}
    };

    initUI();
    initStars();
}

void LobbyListScreen::initUI() {
    titleText = makeText(font, "LOBBIES DISPONIBLES", 42, CYAN);
    auto tb = titleText.getLocalBounds();
    titleText.setOrigin(tb.width/2.f, tb.height/2.f);
    titleText.setPosition(window.getSize().x/2.f, 90.f);

    statusText = makeText(font, "Double-clique pour rejoindre • Clique pour selectionner", 18, sf::Color(200, 200, 220));
    auto sb = statusText.getLocalBounds();
    statusText.setOrigin(sb.width/2.f, 0.f);
    statusText.setPosition(window.getSize().x/2.f, listTopY - 30.f);

    const float cx = window.getSize().x/2.f;
    auto makeBtn = [&](RoundedRectangleShape& r, sf::Text& t, const std::string& label, float x, float y) {
    r.setSize({200.f, 50.f});
    r.setCornersRadius(12.f);
    r.setCornerPointCount(24);
    r.setOrigin(r.getSize().x / 2.f, r.getSize().y / 2.f);
    r.setPosition(x, y);
    r.setFillColor(DARK);
    r.setOutlineColor(CYAN);
    r.setOutlineThickness(2.f);

    t = makeText(font, label, 22, sf::Color::White);
    auto b = t.getLocalBounds();
    t.setOrigin(b.width / 2.f, b.height / 2.f);
    t.setPosition(x, y - 3.f);
};


const float bottomYTop = window.getSize().y - 100.f;
const float bottomYBottom = window.getSize().y - 35.f;
const float spacing = 140.f;
const float spacingRefresh = -10.f;
const float spacingCreate = 210.f;

makeBtn(btnCreate, txtCreate, "Creer un lobby", cx - spacingCreate, bottomYTop - 15);
makeBtn(btnRefresh, txtRefresh, "Rafraichir",   cx + spacingRefresh, bottomYTop - 15);
makeBtn(btnBack, txtBack, "Retour", cx, bottomYBottom +5);


    btnRefresh.move(220.f, 0.f);
    txtRefresh.move(220.f, -2.f);
    btnBack.move(0.f, -10.f);
    txtBack.move(0.f, -10.f);
    joinButtons.clear();
    const float xRight = window.getSize().x / 2.f + listWidth / 2.f - 70.f;

    for (size_t i = 0; i < lobbies.size(); ++i) {
        LobbyButton btn;
        btn.box = RoundedRectangleShape({120.f, 36.f}, 8.f);

        btn.box.setOrigin(btn.box.getSize() / 2.f);
btn.box.setPosition(xRight, listTopY + i * (rowHeight + 6.f) + 28.f);
        btn.box.setFillColor(sf::Color(0, 60, 100, 220));
        btn.box.setOutlineColor(sf::Color(0, 200, 255));
        btn.box.setOutlineThickness(1.9f);

        btn.shadow = RoundedRectangleShape({120.f, 36.f}, 8.f, 40);
        btn.shadow.setOrigin(btn.shadow.getSize() / 2.f);
        btn.shadow.setPosition(xRight + 2.f, listTopY + i * (rowHeight + 6.f) + 28.f);
        btn.shadow.setFillColor(sf::Color(0, 0, 0, 100));

        btn.label.setFont(font);
        btn.label.setString("Rejoindre");
        btn.label.setCharacterSize(16);
        btn.label.setFillColor(sf::Color::White);

        auto bounds = btn.label.getLocalBounds();
        btn.label.setOrigin(bounds.width / 2.f, bounds.height / 2.f);
        btn.label.setPosition(btn.box.getPosition().x, btn.box.getPosition().y - 3.f);

        joinButtons.push_back(btn);
    }

}

void LobbyListScreen::initStars() {
    stars.clear();
    for (int i = 0; i < 120; ++i) {
        Star s;
        s.shape.setRadius(static_cast<float>(rand() % 2 + 1));
        s.shape.setOrigin(s.shape.getRadius(), s.shape.getRadius());
        s.shape.setPosition(
            static_cast<float>(rand() % window.getSize().x),
            static_cast<float>(rand() % window.getSize().y)
        );
        s.speed = static_cast<float>(rand() % 20 + 10);
        s.phase = static_cast<float>((rand() % 100) / 100.f * 6.2831f);
        s.brightness = static_cast<float>((rand() % 50 + 50) / 100.f);

        int type = rand() % 3;
        if (type == 0)
            s.color = sf::Color(255, 240, 180);
        else if (type == 1)
            s.color = sf::Color(200, 220, 255);
        else
            s.color = sf::Color(255, 255, 220);

        stars.push_back(s);
    }
}

void LobbyListScreen::updateStars(float dt) {
    static float time = 0.f;
    time += dt * 2.f;

    for (auto& s : stars) {
        auto pos = s.shape.getPosition();
        pos.x += std::sin(time * 0.3f + s.phase) * 0.15f;
        pos.y += std::cos(time * 0.5f + s.phase) * 0.1f;
        s.shape.setPosition(pos);

        float flicker = 0.5f + 0.5f * std::sin(time * 2.f + s.phase);
        float alpha = 120 + 120 * flicker * s.brightness;

        s.shape.setFillColor(sf::Color(
            s.color.r,
            s.color.g,
            s.color.b,
            static_cast<sf::Uint8>(alpha)
        ));
    }
}


void LobbyListScreen::drawStars(sf::RenderTarget& target) const {
    for (const auto& s : stars) target.draw(s.shape);
}

void LobbyListScreen::drawList(sf::RenderTarget& target)  {
    const float cx = window.getSize().x/2.f;
    const float x  = cx - (listWidth/2.f);
    for (size_t i = 0; i < lobbies.size(); ++i) {
    float y = listTopY + i * (rowHeight + 6.f);

    RoundedRectangleShape row({listWidth, rowHeight - 5.f}, 5.f);
    row.setPosition(x, y + 2.f);
    row.setFillColor(lobbies[i].selected ? sf::Color(25, 45, 70) : sf::Color(15, 25, 45));
    row.setOutlineColor(lobbies[i].selected ? ORANGE : CYAN);
    row.setOutlineThickness(lobbies[i].selected ? 2.5f : 2.f);
    target.draw(row);

    auto name = makeText(font, lobbies[i].name, 22, sf::Color::White);
    name.setPosition(x + 16.f, y + 14.f);
    target.draw(name);

    char buf[64];
    std::snprintf(buf, sizeof(buf), "%d/%d joueurs", lobbies[i].players, lobbies[i].capacity);
    auto info = makeText(font, buf, 18, sf::Color(200, 220, 240));
    auto nb = info.getLocalBounds();
    info.setPosition(x + listWidth - nb.width - 160.f, y + 16.f);
    target.draw(info);

    auto& btn = joinButtons[i];

    float speed = 0.08f;
    if (btn.hovered)
        btn.hoverAnim = std::min(1.f, btn.hoverAnim + speed);
    else
        btn.hoverAnim = std::max(0.f, btn.hoverAnim - speed);

    sf::Color baseFill(0, 60, 100, 220);
    sf::Color hoverFill(20, 100, 160, 255);
    sf::Color baseOutline(0, 200, 255);
    sf::Color hoverOutline(255, 150, 80);

    sf::Color fill(
        baseFill.r + (hoverFill.r - baseFill.r) * btn.hoverAnim,
        baseFill.g + (hoverFill.g - baseFill.g) * btn.hoverAnim,
        baseFill.b + (hoverFill.b - baseFill.b) * btn.hoverAnim,
        255
    );

    sf::Color outline(
        baseOutline.r + (hoverOutline.r - baseOutline.r) * btn.hoverAnim,
        baseOutline.g + (hoverOutline.g - baseOutline.g) * btn.hoverAnim,
        baseOutline.b + (hoverOutline.b - baseOutline.b) * btn.hoverAnim
    );

    btn.box.setFillColor(fill);
    btn.box.setOutlineColor(outline);
    btn.box.setOutlineThickness(1.9f + btn.hoverAnim * 1.9f);

    btn.shadow.setFillColor(sf::Color(0, 0, 0, 60 + 100 * btn.hoverAnim));

    float scale = 1.f + 0.05f * btn.hoverAnim;
    btn.box.setScale(scale, scale);
    btn.label.setScale(scale, scale);

    target.draw(btn.shadow);
    target.draw(btn.box);
    target.draw(btn.label);
}

}

void LobbyListScreen::drawButtons(sf::RenderTarget& target) const {
    auto drawBtn = [&](const RoundedRectangleShape& r, const sf::Text& t){
        target.draw(r);
        target.draw(t);
    };
    drawBtn(btnCreate, txtCreate);
    drawBtn(btnRefresh, txtRefresh);
    drawBtn(btnBack,   txtBack);
}
void LobbyListScreen::handleMouseMove(sf::Vector2f mp) {
    auto hoverAnimEffect = [&](RoundedRectangleShape& r, sf::Text& t, bool& hovered, float& anim) {
        bool inside = r.getGlobalBounds().contains(mp);

        if (inside && !hovered)
            hovered = true;
        else if (!inside && hovered)
            hovered = false;

        float speed = 0.08f;
        anim = hovered ? std::min(1.f, anim + speed) : std::max(0.f, anim - speed);

        sf::Color baseFill(20, 30, 50, 220);
        sf::Color hoverFill(40, 90, 130, 255);
        sf::Color baseOutline(0, 200, 255);
        sf::Color hoverOutline(255, 150, 80);

        sf::Color fill(
            baseFill.r + (hoverFill.r - baseFill.r) * anim,
            baseFill.g + (hoverFill.g - baseFill.g) * anim,
            baseFill.b + (hoverFill.b - baseFill.b) * anim,
            255
        );

        sf::Color outline(
            baseOutline.r + (hoverOutline.r - baseOutline.r) * anim,
            baseOutline.g + (hoverOutline.g - baseOutline.g) * anim,
            baseOutline.b + (hoverOutline.b - baseOutline.b) * anim
        );

        r.setFillColor(fill);
        r.setOutlineColor(outline);
        r.setOutlineThickness(2.f + anim);
        r.setScale(1.f + 0.05f * anim, 1.f + 0.05f * anim);

        t.setFillColor(sf::Color(255, 255, 255, 200 + 55 * anim));
    };

    hoverAnimEffect(btnCreate, txtCreate, createHovered, createAnim);
    hoverAnimEffect(btnRefresh, txtRefresh, refreshHovered, refreshAnim);
    hoverAnimEffect(btnBack, txtBack, backHovered, backAnim);

    for (auto& btn : joinButtons) {
        bool inside = btn.box.getGlobalBounds().contains(mp);
        btn.hovered = inside;
    }
}


void LobbyListScreen::handleClick(sf::Vector2f mp, LobbyListResult& out) {

    if (btnRefresh.getGlobalBounds().contains(mp)) {
    out = LobbyListResult::Refresh;
    return;
    }
    if (btnCreate.getGlobalBounds().contains(mp)) {
        out = LobbyListResult::Create; return;
    }
    if (btnBack.getGlobalBounds().contains(mp)) {
        out = LobbyListResult::Back; return;
    }

    const float cx = window.getSize().x/2.f;
    const float x  = cx - (listWidth/2.f);

    static sf::Clock dblClk;
    static int lastSel = -1;

    for (size_t i = 0; i < joinButtons.size(); ++i) {
        if (joinButtons[i].box.getGlobalBounds().contains(mp)) {
            selectedIndex = (int)i;
            out = LobbyListResult::Join;
        return;
        }
    }

    for (size_t i=0; i<lobbies.size(); ++i) {
        float y = listTopY + i*rowHeight;
        sf::FloatRect row(x, y, listWidth, rowHeight-8.f);
        if (row.contains(mp)) {
            for (auto& it : lobbies) it.selected = false;
            lobbies[i].selected = true;
            selectedIndex = static_cast<int>(i);

            if (lastSel == (int)i && dblClk.getElapsedTime().asMilliseconds() < 350) {
                out = LobbyListResult::Join;
            }
            lastSel = (int)i;
            dblClk.restart();
            break;
        }
    }
}

StaticLobbyItem LobbyListScreen::getSelectedLobby() const {
    if (selectedIndex >= 0 && selectedIndex < (int)lobbies.size())
        return lobbies[selectedIndex];
    return {"",0,0,false};
}

LobbyListResult LobbyListScreen::run() {
    LobbyListResult result = LobbyListResult::None;
    sf::Clock clk;

    while (window.isOpen() && result == LobbyListResult::None) {
        sf::Event e;
        while (window.pollEvent(e)) {
            if (e.type == sf::Event::Closed) return LobbyListResult::Back;

            if (e.type == sf::Event::MouseMoved) {
                sf::Vector2f mp = window.mapPixelToCoords({e.mouseMove.x, e.mouseMove.y});
                handleMouseMove(mp);
            }
            if (e.type == sf::Event::MouseButtonPressed && e.mouseButton.button == sf::Mouse::Left) {
                sf::Vector2f mp = window.mapPixelToCoords({e.mouseButton.x, e.mouseButton.y});
                handleClick(mp, result);
            }
            if (e.type == sf::Event::KeyPressed) {
                if (e.key.code == sf::Keyboard::Escape) result = LobbyListResult::Back;
                if (e.key.code == sf::Keyboard::Enter && selectedIndex >= 0) result = LobbyListResult::Join;
                if (e.key.code == sf::Keyboard::C) result = LobbyListResult::Create;
                if (e.key.code == sf::Keyboard::Up || e.key.code == sf::Keyboard::Down) {
                    if (!lobbies.empty()) {
                        if (selectedIndex < 0) selectedIndex = 0;
                        else {
                            int dir = (e.key.code == sf::Keyboard::Up) ? -1 : +1;
                            selectedIndex = (selectedIndex + dir + (int)lobbies.size()) % (int)lobbies.size();
                        }
                        for (auto& it : lobbies) it.selected = false;
                        lobbies[selectedIndex].selected = true;
                    }
                }
            }
        }

        float dt = clk.restart().asSeconds();
        updateStars(dt);

sf::VertexArray gradient(sf::Quads, 4);
gradient[0].position = {0, 0};
gradient[1].position = {(float)window.getSize().x, 0};
gradient[2].position = {(float)window.getSize().x, (float)window.getSize().y};
gradient[3].position = {0, (float)window.getSize().y};

gradient[0].color = sf::Color(5, 5, 10);
gradient[1].color = sf::Color(10, 10, 25);
gradient[2].color = sf::Color(15, 10, 40);
gradient[3].color = sf::Color(5, 5, 15);

window.draw(gradient);

        drawStars(window);
        sf::Text glow = titleText;
        glow.setFillColor(sf::Color(0, 180, 255, 40));
        glow.setScale(1.015f, 1.015f);
        glow.move(0, 2.f);
        window.draw(glow);

        window.draw(titleText);
        window.draw(statusText);
        drawList(window);
        drawButtons(window);
        window.display();
    }
    return result;
}
