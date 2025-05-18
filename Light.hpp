#ifndef LIGHT_HPP
#define LIGHT_HPP

#include "./Vec3.hpp"
#include <vector>

struct Light {
  float ambient;
  float diffuse;
  
  std::vector<Vec3> pointLights;
  std::vector<Vec3> directionLights;
};

#endif