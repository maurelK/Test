#pragma once
#include <SFML/Graphics.hpp>
#include <cmath>

class RoundedRectangleShape : public sf::Shape {
public:
    explicit RoundedRectangleShape(const sf::Vector2f& size = sf::Vector2f(0, 0), float radius = 0, std::size_t cornerPointCount = 16)
        : m_size(size), m_radius(radius), m_cornerPointCount(cornerPointCount) {
        update();
    }

    void setSize(const sf::Vector2f& size) { m_size = size; update(); }
    void setCornersRadius(float radius) { m_radius = radius; update(); }
    void setCornerPointCount(std::size_t count) { m_cornerPointCount = count; update(); }

    const sf::Vector2f& getSize() const { return m_size; }
    float getCornersRadius() const { return m_radius; }
    std::size_t getCornerPointCount() const { return m_cornerPointCount; }

    virtual std::size_t getPointCount() const override { return m_cornerPointCount * 4; }

    virtual sf::Vector2f getPoint(std::size_t index) const override {
        static const float pi = 3.141592654f;
        std::size_t corner = index / m_cornerPointCount;
        float angle = (index % m_cornerPointCount) * pi / 2 / (m_cornerPointCount - 1);

        sf::Vector2f center;
        switch (corner) {
            case 0: center = {m_size.x - m_radius, m_radius}; break;
            case 1: center = {m_radius, m_radius}; break;
            case 2: center = {m_radius, m_size.y - m_radius}; break;
            case 3: center = {m_size.x - m_radius, m_size.y - m_radius}; break;
        }

        return center + sf::Vector2f(std::cos(angle + corner * pi / 2) * m_radius,
                                     -std::sin(angle + corner * pi / 2) * m_radius);
    }

private:
    sf::Vector2f m_size;
    float m_radius;
    std::size_t m_cornerPointCount;
};
