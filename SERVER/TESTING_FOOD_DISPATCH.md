# Testing Plan for Food Dispatching and Server Command Handling

This document outlines detailed test cases to thoroughly verify the food dispatching and server command handling in the Zappy server.

## 1. Resource Spawning and Distribution
- Verify that at server start, at least one unit of each resource (food, linemate, deraumere, sibur, mendiane, phiras, thystame) is present on the map.
- Verify that resources are evenly spread across the map.
- Verify that resources respawn every 20 time units according to the formula: map_width * map_height * density.
- Verify that food density matches the expected value (e.g., 0.5).

## 2. Handling "Take" Command
- Test taking food from a tile with available food:
  - Player inventory increases by 1 unit of food.
  - Tile resource decreases by 1 unit of food.
  - Server responds with "ok\n".
- Test taking food from a tile with no food:
  - Player inventory remains unchanged.
  - Server responds with "ko\n".
- Test taking invalid resource name:
  - Server responds with "ko\n".

## 3. Handling "Set" Command
- Test setting food from player inventory to tile:
  - Player inventory decreases by 1 unit of food.
  - Tile resource increases by 1 unit of food.
  - Server responds with "ok\n".
- Test setting food not in player inventory:
  - Server responds with "ko\n".
- Test setting invalid resource name:
  - Server responds with "ko\n".

## 4. Decrease of Life Units and Food Consumption
- Verify that player life units decrease by 1 every time unit.
- Verify that when life units reach 0, one unit of food is consumed from player inventory.
- Verify that life units reset to 126 after food consumption.
- Verify that if no food is available, player death is triggered.

## 5. Player Death Handling
- Verify that when player dies:
  - Server sends "dead\n" to the client.
  - Player socket is closed.
  - Player is marked invalid.
- Verify that player death due to starvation is logged.

## 6. Server Responses
- Verify that server responses "ok\n", "ko\n", and "dead\n" are sent appropriately for commands and events.

## 7. Command Queueing and Execution Timing
- Verify that commands are queued correctly up to 10 commands per player.
- Verify that commands execute after the correct delay based on frequency.
- Verify that unknown commands receive "ko\n" response.

## 8. Edge Cases
- Test invalid resource names in commands.
- Test commands with malformed input.
- Test command queue overflow (more than 10 commands).
- Test player actions at map boundaries (wrapping around).

---

Use this plan to perform manual or automated testing to ensure full coverage and correctness of the food dispatching and server command handling.
