
import argparse
import sys
import socket
import select
import time
import re
import random
 
class Config:
    """
    Configuration class for Zappy AI - All constants and game rules
    """
    
    INSULTS = [
        'Noob detected', 'Git gud scrub', 'Ez clap', 'Skill issue', 
        'Trash player', 'Uninstall pls', 'Bot behavior', 'Hardstuck bronze',
        'No brain detected', 'Imagine being you', 'Cringe gameplay', 'L + ratio'
    ]
    
    ELEVATION_REQUIREMENTS = {
        1: {"players": 1, "linemate": 1, "deraumere": 0, "sibur": 0, "mendiane": 0, "phiras": 0, "thystame": 0},
        2: {"players": 2, "linemate": 1, "deraumere": 1, "sibur": 1, "mendiane": 0, "phiras": 0, "thystame": 0},
        3: {"players": 2, "linemate": 2, "deraumere": 0, "sibur": 1, "mendiane": 0, "phiras": 2, "thystame": 0},
        4: {"players": 4, "linemate": 1, "deraumere": 1, "sibur": 2, "mendiane": 0, "phiras": 1, "thystame": 0},
        5: {"players": 4, "linemate": 1, "deraumere": 2, "sibur": 1, "mendiane": 3, "phiras": 0, "thystame": 0},
        6: {"players": 6, "linemate": 1, "deraumere": 2, "sibur": 3, "mendiane": 0, "phiras": 1, "thystame": 0},
        7: {"players": 6, "linemate": 2, "deraumere": 2, "sibur": 2, "mendiane": 2, "phiras": 2, "thystame": 1}
    }
    
    SURVIVAL_THRESHOLDS = {
        'food': 10,
        'linemate': 3,
        'deraumere': 2,    
        'sibur': 3,        
        'mendiane': 4,
        'phiras': 3,       
        'thystame': 2,
    }
    
    RESOURCE_PRIORITIES = {
        'food': 10,
        'thystame': 9,
        'phiras': 8,
        'mendiane': 7,
        'sibur': 6,
        'deraumere': 5,
        'linemate': 4,
    }
    
    RESOURCES = ['food', 'linemate', 'deraumere', 'sibur', 'mendiane', 'phiras', 'thystame']
    STONES = ['linemate', 'deraumere', 'sibur', 'mendiane', 'phiras', 'thystame']
    
    MOVEMENT_PATTERNS = {
        0: [],                                               # Current tile (no movement needed)
        1: ['Forward'],                                      # Tile in front
        2: ['Forward', 'Right', 'Forward'],                 # Tile front-right
        3: ['Right', 'Forward'],                            # Tile right
        4: ['Right', 'Right', 'Forward'],                   # Tile back-right
        5: ['Right', 'Right'],                              # Tile behind
        6: ['Left', 'Left', 'Forward'],                     # Tile back-left
        7: ['Left', 'Forward'],                             # Tile left
        8: ['Forward', 'Left', 'Forward'],                  # Tile front-left
        9: ['Forward', 'Forward', 'Left', 'Left', 'Forward'],
        10: ['Forward', 'Forward', 'Left', 'Forward'],
        11: ['Forward', 'Forward', 'Forward'],
        12: ['Forward', 'Forward', 'Right', 'Forward'],
        13: ['Forward', 'Forward', 'Right', 'Right', 'Forward'],
        14: ['Forward', 'Forward', 'Left', 'Left'],
        15: ['Forward', 'Forward', 'Left'],
    }
    
    FOOD_SURVIVAL_TIME = 126
    STARTING_FOOD = 10 
    RESOURCE_SPAWN_INTERVAL = 20
    
    COMMAND_TIMES = {
        'Forward': 7,
        'Right': 7, 
        'Left': 7,
        'Look': 7,
        'Inventory': 1,
        'Broadcast': 7,
        'Connect_nbr': 0,
        'Fork': 42,
        'Eject': 7,
        'Take': 7,
        'Set': 7,
        'Incantation': 300
    }
    
    class States:
        SPAWNING = "spawning"
        EXPLORING = "exploring" 
        COLLECTING = "collecting"
        SEEKING_TEAMMATES = "seeking_teammates"
        PREPARING_ELEVATION = "preparing_elevation"
        ELEVATING = "elevating"
        EMERGENCY_FOOD = "emergency_food"
        REPRODUCING = "reproducing"
        GRIEFING = "griefing"  # For DDOS tactics
    
    BROADCAST_MESSAGES = {
        'food_request': "NEED_FOOD_URGENTLY",
        'elevation_call': "ELEVATION_LVL_{level}_AT_{x}_{y}",
        'resource_found': "FOUND_{resource}_AT_{x}_{y}",
        'danger_warning': "DANGER_ENEMIES_AT_{x}_{y}",
        'teammate_request': "NEED_TEAMMATES_LVL_{level}",
        'insult': "{insult}_GET_REKT"
    }
    
    MAX_INVENTORY_SAFETY = 50   # Don't overload inventory
    TEAM_COORDINATION_DISTANCE = 5  # Max distance to coordinate with teammates
    EXPLORATION_RANDOMNESS = 0.3
    GRIEF_FREQUENCY = 10
    
    # Win condition
    WINNING_PLAYERS_REQUIRED = 6    # Need 6 players at max level to win
    MAX_LEVEL = 8                   # Maximum level (7->8 is final elevation)