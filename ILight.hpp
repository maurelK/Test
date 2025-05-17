#ifndef ILIGHT_HPP
#define ILIGHT_HPP

#include "Vec3.hpp"

class ILight {
public:
    virtual ~ILight() = default;
};

class AmbientLight : public ILight {
public:
    float intensity;
    AmbientLight(float intensity) : intensity(intensity) {}
};

class DiffuseLight : public ILight {
public:
    float diffusion;
    DiffuseLight(float intensity) : diffusion(intensity) {}
};

class PointLight : public ILight {
public:
    Vec3 position;
    PointLight(const Vec3& position) : position(position) {}
};

class DirectionalLight : public ILight {
public:
    Vec3 direction;
    DirectionalLight(const Vec3& direction) : direction(direction) {}
};

#endif // ILIGHT_HPP
