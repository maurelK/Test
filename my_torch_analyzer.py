#!/usr/bin/env python3

import sys
import argparse
from my_torch.Neuron import Neuron
import numpy as np

# Simple FEN encoder
PIECE_MAP = {
    'P': 1, 'N': 2, 'B': 3, 'R': 4, 'Q': 5, 'K': 6,
    'p': -1, 'n': -2, 'b': -3, 'r': -4, 'q': -5, 'k': -6,
}

def fen_to_vector(fen):
    """Convert FEN string to 64-element vector"""
    board_part = fen.split()[0]  # Only take the board part
    vector = []

    for char in board_part:
        if char.isdigit():
            # Add zeros for empty squares
            vector.extend([0] * int(char))
        elif char == '/':
            # Skip row separators
            continue
        elif char in PIECE_MAP:
            vector.append(PIECE_MAP[char])

    # Ensure exactly 64 squares (pad with zeros if needed)
    while len(vector) < 64:
        vector.append(0)

    # Truncate if too long (shouldn't happen with valid FEN)
    vector = vector[:64]

    return np.array(vector).reshape(-1, 1)

def vector_to_label(output, fen):
    """Convert network output to label with color information"""
    classes = ['Nothing', 'Check', 'Checkmate']
    idx = np.argmax(output)
    base_label = classes[idx]
    
    if base_label == 'Nothing':
        return 'Nothing'
    
    # Determine whose turn it is from FEN
    fen_parts = fen.split()
    if len(fen_parts) >= 2:
        turn = fen_parts[1]  # 'w' or 'b'
        color = 'White' if turn == 'w' else 'Black'
        return f"{base_label} {color}"
    
    # Fallback if FEN parsing fails
    return base_label

def main():
    parser = argparse.ArgumentParser(description='MY_TORCH Chess Analyzer')
    parser.add_argument('--train', action='store_true', help='Train mode')
    parser.add_argument('--predict', action='store_true', help='Predict mode')
    parser.add_argument('--save', type=str, help='Save file for trained network')
    parser.add_argument('loadfile', help='Network file to load')
    parser.add_argument('chessfile', help='Chess file with FEN strings')

    args = parser.parse_args()

    if not (args.train or args.predict):
        print("Error: Must specify --train or --predict", file=sys.stderr)
        sys.exit(84)

    try:
        network = Neuron.load(args.loadfile)
    except FileNotFoundError:
        print(f"Error: Network file {args.loadfile} not found", file=sys.stderr)
        sys.exit(84)

    with open(args.chessfile, 'r') as f:
        lines = f.readlines()

    if args.train:
        # Assume lines are "FEN expected_output"
        X = []
        y = []
        for line in lines:
            parts = line.strip().split()
            if len(parts) >= 2:
                fen = parts[0]
                label = parts[1]
                X.append(fen_to_vector(fen).flatten())
                if label == 'Nothing':
                    y.append([1, 0, 0])
                elif label == 'Check':
                    y.append([0, 1, 0])
                elif label == 'Checkmate':
                    y.append([0, 0, 1])
        X = np.array(X).T
        y = np.array(y).T
        network.train(X, y, epochs=100)
        savefile = args.save if args.save else args.loadfile
        network.save(savefile)
        print(f"Training completed. Network saved to {savefile}")

    elif args.predict:
        for line in lines:
            fen = line.strip().split()[0]
            X = fen_to_vector(fen)
            output = network.predict(X)
            label = vector_to_label(output, fen)
            print(label)

if __name__ == '__main__':
    main()