#!/usr/bin/env python3
import argparse
import sys
import socket
import select
import time
from config import Config
import re   
import random

class Connection:
    def __init__(self, port, host):
        self._port = port
        self._host = host
        self._socket = None
        self._buffer = ""

    def connect(self):
        try:
            self._socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            self._socket.settimeout(5)
            self._socket.connect((self._host, self._port))
            self._socket.setblocking(False)
        except Exception as e:
            print(f"Connection error: {e}")
            return False
        return True

    def send(self, message):
        try:
            self._socket.sendall(message.encode())
        except Exception as e:
            print(f"Send error: {e}")

    def receive(self):
        while '\n' not in self._buffer:
            try:
                ready = select.select([self._socket], [], [], 5)
                if ready[0]:
                    data = self._socket.recv(4096).decode()
                    if not data:
                        raise ConnectionError("Connection closed by server")
                    self._buffer += data
                else:
                    return None
            except Exception as e:
                print(f"Error in receive: {e}")
                return None
        end_line = self._buffer.find('\n')
        line = self._buffer[:end_line+1]
        self._buffer = self._buffer[end_line+1:]
        return line.strip()

    def close(self):
        if self._socket:
            self._socket.close()
            print("Connection closed.")

class AI:
    def __init__(self, socket):
        self.socket = socket
        self.level = 1
        self.connect_nbr = 0
        self.fork_done = False

    def make_decision(self):
        """Logique principale complète"""
        if self.socket.pend_com >= 10:
            return
        self.update_inventory()
        self.manage_inventory()
        self.check_connections()
        self.eject_competitors()
        if self.should_fork():
            self.socket.send_command("Fork")
            self.fork_done = True
            return
        if self.can_level_up():
            print(f"Ready to level up from {self.level} to {self.level + 1}!")
            self.socket.send_command("Incantation")
            return
        self.collect_needed_resources()

    def update_inventory(self):
        if not self.socket.inventory:
            self.socket.send_command("Inventory")

    def manage_inventory(self):
        """Jette l'excédent selon les seuils de sécurité"""
        for resource, current_amount in self.socket.inventory.items():
            threshold = Config.SURVIVAL_THRESHOLDS.get(resource, 1)
            if current_amount > threshold:
                print(f" Dropping excess {resource} (keeping {threshold})")
                self.socket.send_command(f"Set {resource}")
                break 

    def check_connections(self):
        """Vérifie les slots d'équipe disponibles"""
        self.socket.send_command("Connect_nbr")

    def eject_competitors(self):
        """Éjecte les autres joueurs de notre case"""
        if len(self.socket.visible_tiles) > 0:
            current_tile = self.socket.visible_tiles[0]
            if current_tile.count('player') > 1:
                print("Ejecting competitors!")
                self.socket.send_command("Eject")
                insult = random.choice(Config.INSULTS)
                self.socket.send_command(f"Broadcast {insult}")

    def should_fork(self):
        """Détermine si on doit se reproduire"""
        food = self.socket.inventory.get('food', 0)
        conditions = [
            self.connect_nbr > 0,
            food > 15,
            not self.fork_done
        ]
        return all(conditions)

    def can_level_up(self):
        """Vérifie si on a tout pour l'élévation"""
        if self.level not in Config.ELEVATION_REQUIREMENTS:
            return False
        requirements = Config.ELEVATION_REQUIREMENTS[self.level]
        for resource, needed in requirements.items():
            if resource == "players":
                continue
            current = self.socket.inventory.get(resource, 0)
            if current < needed:
                print(f" Missing {resource}: have {current}, need {needed}")
                return False
        print(f"Have all resources for level {self.level} → {self.level + 1}")
        return True

    def get_needed_resource(self):
        """Trouve la ressource la plus prioritaire à collecter"""
        if self.level not in Config.ELEVATION_REQUIREMENTS:
            return "food"
        requirements = Config.ELEVATION_REQUIREMENTS[self.level]
        missing_resources = []
        for resource, needed in requirements.items():
            if resource == "players":
                continue
            current = self.socket.inventory.get(resource, 0)
            if current < needed:
                priority = Config.RESOURCE_PRIORITIES.get(resource, 1)
                missing_resources.append((resource, priority))
        
        if missing_resources:
            # Trier par priorité (plus haute d'abord)
            missing_resources.sort(key=lambda x: x[1], reverse=True)
            return missing_resources[0][0]
        return None
    
    def collect_needed_resources(self):
        """Collecte intelligente basée sur les besoins"""
        needed = self.get_needed_resource()
        if not needed:
            self.explore()
            return
        # Chercher la ressource
        self.socket.send_command("Look")
        for tile_index, tile_content in enumerate(self.socket.visible_tiles):
            if needed in tile_content:
                print(f"📍 Found {needed} at tile {tile_index}")
                self.move_to_tile(tile_index, needed)
                return
            print(f" {needed} not visible, exploring...")
        self.explore()

    def move_to_tile(self, tile_index, resource):
        """Se déplace vers une case et ramasse la ressource"""
        if tile_index == 0:
            # Déjà sur la case
            self.socket.send_command(f"Take {resource}")
            return
        if tile_index in Config.MOVEMENT_PATTERNS:
            moves = Config.MOVEMENT_PATTERNS[tile_index]
            print(f"Moving to tile {tile_index}: {moves}")
            for move in moves:
                self.socket.send_command(move)
                self.socket.send_command(f"Take {resource}")
        else:
            print(f"No movement pattern for tile {tile_index}")

    def explore(self):
        """Exploration intelligente"""
        # Mouvement aléatoire pour éviter les boucles
        moves = ['Forward', 'Left', 'Right']
        self.socket.send_command(random.choice(moves))

    def handle_level_up(self, response):
        """Traite la réponse d'élévation"""
        if "Current level" in response:
            parts = response.split()
            for i, part in enumerate(parts):
                if part == "level:" and i + 1 < len(parts):
                    self.level = int(parts[i + 1])
                    print(f"🎉 Level up successful! New level: {self.level}")
                    self.socket.send_command(f"Broadcast Level {self.level} achieved!")
                    break
        else:
            print("levation failed")





class Socket:
    def __init__(self, port, host, team_name):
        self.connection = Connection(port, host)
        self._team_name = team_name
        self.inventory = {}
        self.map_dimensions = (0, 0)
        self.command_queue = []
        self.pend_com = 0
        self.last_command_time = 0
        self.command_cooldown = 0.1
        self.visible_tiles = []
        self.ai = AI(self)

    def connect(self):
        return self.connection.connect()

    def send(self, message):
        self.connection.send(message)
        self.pend_com += 1
        self.last_command_time = time.time()

    def receive(self):
        return self.connection.receive()

    def handshake(self):
        mesg = self.receive()
        print(f"Received: {repr(mesg)}")
        if mesg != "WELCOME":
            print("Error: not server auth")
            return False
        self.send(f"{self._team_name}\n")
        slots_available = self.receive()
        print(f"Slots available: {repr(slots_available)}")
        if not slots_available or not slots_available.isdigit():
            print("Error: team refused or socket closed")
            return False
        dimensions = self.receive()
        print(f"Map dimensions: {repr(dimensions)}")
        dims = dimensions.split()
        if len(dims) == 2 and dims[0].isdigit() and dims[1].isdigit():
            self.map_dimensions = (int(dims[0]), int(dims[1]))
        return True

#    def send_command(self, command):
#        if self.pend_com < 10:
#            self.send(command + '\n')
#        else:
#            self.command_queue.append(command)

    def send_command(self, command):
        if self.pend_com < 10:
            print(f"[SEND] {command}")
            self.send(command + '\n')
        else:
            self.command_queue.append(command)

    def process_command_queue(self):
        while self.command_queue and self.pend_com < 10:
            cmd = self.command_queue.pop(0)
            self.send(cmd + '\n')

    def handle_response(self, response):
        if response == "dead":
            print("You are dead. Exiting.")
            self.close()
            sys.exit(0)
        if response in ("ok", "ko"):
            self.pend_com = max(0, self.pend_com - 1)
        elif response.startswith('['):
            if self.is_inventory_response(response):
                self.parse_inventory(response)
            else:
                self.parse_look(response)
        else:
            print(f"Server message: {response}")

    def is_inventory_response(self, response):
        inventory_keys = {'food', 'linemate', 'deraumere', 'sibur', 'mendiane', 'phiras', 'thystame'}
        response_clean = response.strip('[] \n')
        tokens = re.split(r'[,\s]+', response_clean)  # autorise virgules et espaces

        if len(tokens) % 2 != 0:
            return False

        for i in range(0, len(tokens), 2):
            if tokens[i] not in inventory_keys:
                return False
            try:
                int(tokens[i+1])
            except ValueError:
                return False
        return True


    def parse_inventory(self, inv_str):
        import re
        tokens = re.split(r'[,\s]+', inv_str.strip('[] \n'))
        self.inventory = {tokens[i]: int(tokens[i+1]) for i in range(0, len(tokens), 2)}
        print(f"Updated inventory: {self.inventory}")

    def parse_look(self, look_str):
        tiles = [tile.strip().split() for tile in look_str.strip('[] \n').split(',')]
        self.visible_tiles = tiles
        print(f"Parsed look: {self.visible_tiles}")

    def run_loop(self):
        print("Entering main loop. Press Ctrl+C to exit.")
        try:
            while True:
                data = self.receive()
                if data:
                    self.handle_response(data)
                    self.process_command_queue()
                    self.ai.make_decision()
                else:
                    now = time.time()
                    if now - self.last_command_time > self.command_cooldown:
                        self.send_command("Inventory")
                        self.last_command_time = now
                    time.sleep(0.05)
        except KeyboardInterrupt:
            print("Exiting main loop.")
        except ConnectionError as e:
            print(f"Connection error in run_loop: {e}")
            self.close()
            sys.exit(84)

    def close(self):
        self.connection.close()


def parse_args():
    parser = argparse.ArgumentParser(add_help=False)
    parser.add_argument('-p', dest='port', type=int, required=True)
    parser.add_argument('-n', dest='team', type=str, required=True)
    parser.add_argument('-h', dest='host', type=str, required=True)
    parser.add_argument('-help', action='store_true', help='Afficher l’aide')
    args = parser.parse_args()

    if args.help:
        print("USAGE: ./zappy_ai -p port -n name -h machine")
        sys.exit(0)
    return args

if __name__ == '__main__':
    av = parse_args()
    client = Socket(av.port, av.host, av.team)
    if not client.connect() or not client.handshake():
        sys.exit(84)
    client.send_command("Inventory")
    client.run_loop()
    client.close()
