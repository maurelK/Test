#pragma once
#include <cstdint>
#include <bitset>

using Entity = uint32_t;
constexpr Entity MAX_ENTITIES = 10000;

using ComponentType = uint8_t;
constexpr ComponentType MAX_COMPONENTS = 32;

using Signature = std::bitset<MAX_COMPONENTS>;

// Forward declaration for ComponentStorage
template<typename T>
class ComponentStorage;

// Alias for compatibility
template<typename T>
using ComponentArray = ComponentStorage<T>;
