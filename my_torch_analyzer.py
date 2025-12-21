#!/usr/bin/env python3

import sys
import argparse
import numpy as np
from my_torch.Neuron import Neuron

# One-hot encoding: 12 pieces × 64 squares = 768 inputs
PIECE_MAP = {
    'P': 0, 'N': 1, 'B': 2, 'R': 3, 'Q': 4, 'K': 5,
    'p': 6, 'n': 7, 'b': 8, 'r': 9, 'q': 10, 'k': 11
}

def fen_to_vector(fen: str) -> np.ndarray:
    """Convert FEN string to a (768, 1) one-hot encoded vector"""
    board_part = fen.split()[0]
    vector = np.zeros(768)
    square_idx = 0

    for char in board_part:
        if char.isdigit():
            square_idx += int(char)
        elif char == '/':
            continue
        elif char in PIECE_MAP:
            channel = PIECE_MAP[char]
            vector[channel * 64 + square_idx] = 1
            square_idx += 1

    if square_idx != 64:
        raise ValueError("Invalid FEN board encoding")

    return vector.reshape(-1, 1)

def vector_to_label(output: np.ndarray, fen: str) -> str:
    classes = ['Nothing', 'Check', 'Checkmate']
    output = output.flatten()
    idx = int(np.argmax(output))
    base_label = classes[idx]

    # ajouter un seuil pour éviter la surestimation
    if output[0] > 0.7:  # si Nothing est fort
        return 'Nothing'

    if base_label == 'Nothing':
        return 'Nothing'

    parts = fen.split()
    if len(parts) >= 2:
        color = 'White' if parts[1] == 'w' else 'Black'
        return f"{base_label} {color}"

    return base_label


def main():
    parser = argparse.ArgumentParser(description='MY_TORCH Chess Analyzer')
    parser.add_argument('--train', action='store_true', help='Train the neural network')
    parser.add_argument('--predict', action='store_true', help='Predict from FEN positions')
    parser.add_argument('--save', type=str, help='Save trained network to file')
    parser.add_argument('loadfile', help='Neural network file (.nn)')
    parser.add_argument('chessfile', help='File containing chess positions')
    args = parser.parse_args()

    # Validation des options
    if not (args.train or args.predict):
        print("Error: --train or --predict must be specified", file=sys.stderr)
        sys.exit(84)

    if args.train and args.predict:
        print("Error: cannot use --train and --predict together", file=sys.stderr)
        sys.exit(84)

    if args.predict and args.save:
        print("Error: --save is only allowed in train mode", file=sys.stderr)
        sys.exit(84)
    # Chargement du réseau
    try:
        network = Neuron.load(args.loadfile)
    except Exception:
        print(f"Error: cannot load network file '{args.loadfile}'", file=sys.stderr)
        sys.exit(84)

    # Chargement du dataset
    try:
        with open(args.chessfile, 'r') as f:
            lines = f.readlines()
    except Exception:
        print(f"Error: cannot read chess file '{args.chessfile}'", file=sys.stderr)
        sys.exit(84)


    # =========================
    # TRAIN MODE
    # =========================
    if args.train:
        X, y = [], []

        for line in lines:
            line = line.strip()
            if not line:
                continue

            parts = line.split()
            if len(parts) < 7:
                continue

            # Label parsing
            if parts[-1] == 'Nothing':
                fen = ' '.join(parts[:-1])
                label = 'Nothing'
            else:
                fen = ' '.join(parts[:-2])
                label = ' '.join(parts[-2:])

            try:
                vec = fen_to_vector(fen).flatten()
            except Exception:
                continue

            if label == 'Nothing':
                X.append(vec)
                y.append([1, 0, 0])
            elif label.startswith('Checkmate'):
                X.append(vec)
                y.append([0, 0, 1])
            elif label.startswith('Check'):
                X.append(vec)
                y.append([0, 1, 0])

        if not X or not y or len(X) != len(y):
            print("Error: invalid or empty training dataset", file=sys.stderr)
            sys.exit(84)

        X = np.array(X).T
        y = np.array(y).T

        network.train(X, y, epochs=100)

        savefile = args.save if args.save else args.loadfile
        network.save(savefile)
        print(f"Training completed. Network saved to '{savefile}'")

    # =========================
    # PREDICT MODE
    # =========================
    elif args.predict:
        for line in lines:
            line = line.strip()
            if not line:
                continue

            parts = line.split()
            if len(parts) < 6:
                continue

            # Retirer le label s'il existe
            if parts[-1] == 'Nothing':
                fen = ' '.join(parts[:-1])
            elif parts[-2] in ('Check', 'Checkmate'):
                fen = ' '.join(parts[:-2])
            else:
                fen = line  # FEN pur (sans label)

            try:
                X = fen_to_vector(fen)
                output = network.forward(X, training=False) 
                label = vector_to_label(output, fen)
                print(label)
            except Exception:
                continue


if __name__ == '__main__':
    main()
