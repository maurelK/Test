#!/usr/bin/env python3

import sys
import argparse
from my_torch.Neuron import Neuron
import numpy as np



def fen_to_vector(fen):
    """Correct FEN to one-hot encoding"""
    board_part = fen.split()[0]
    piece_map = {
        'P': 0, 'N': 1, 'B': 2, 'R': 3, 'Q': 4, 'K': 5,
        'p': 6, 'n': 7, 'b': 8, 'r': 9, 'q': 10, 'k': 11
    }
    
    vector = np.zeros(768)
    
    row = 0
    col = 0
    
    for char in board_part:
        if char == '/':
            row += 1
            col = 0
        elif char.isdigit():
            col += int(char)
        elif char in piece_map:
            square_idx = row * 8 + col
            channel = piece_map[char]
            vector[channel * 64 + square_idx] = 1
            col += 1
    
    return vector.reshape(-1, 1)

def vector_to_label(output, fen):
    """Convert network output to label with color information"""
    # Get base class
    output = output.flatten()
    class_idx = np.argmax(output)
    
    # Map to class name
    if len(output) == 3:  # 3-class: Nothing, Check, Checkmate
        classes = ['Nothing', 'Check', 'Checkmate']
        base_label = classes[class_idx]
    elif len(output) == 2:  # 2-class: Nothing, Check (for phase 1)
        classes = ['Nothing', 'Check']
        base_label = classes[class_idx]
    else:
        base_label = 'Nothing'
    
    if base_label == 'Nothing':
        return 'Nothing'
    
    # Determine color from FEN
    fen_parts = fen.split()
    if len(fen_parts) >= 2:
        turn = fen_parts[1]  # 'w' or 'b'
        # In check/checkmate, the player whose turn it is is in check
        color = 'White' if turn == 'w' else 'Black'
        return f"{base_label} {color}"
    
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
        # Assume lines are "FEN expected_output" where expected_output can be "Nothing" or "Check Color" or "Checkmate Color"
        X = []
        y = []
        for line in lines:
            line = line.strip()
            if not line:
                continue
                
            parts = line.split()
            if len(parts) < 7:  # Need at least FEN parts + label
                continue
            
            # FEN is first 6 parts, label is last 2 parts (type + color) or 1 part (Nothing)
            if parts[-1] == 'Nothing':
                fen = ' '.join(parts[:-1])
                label = 'Nothing'
            else:
                fen = ' '.join(parts[:-2])
                label = ' '.join(parts[-2:])
            
            X.append(fen_to_vector(fen).flatten())
            
            # Parse label
            if label == 'Nothing':
                y.append([1, 0, 0])
            elif label.startswith('Check'):
                y.append([0, 1, 0])
            elif label.startswith('Checkmate'):
                y.append([0, 0, 1])
        
        if X and y:
            X = np.array(X).T
            y = np.array(y).T
            network.train(X, y, epochs=100)
            savefile = args.save if args.save else args.loadfile
            network.save(savefile)
            print(f"Training completed. Network saved to {savefile}")
        else:
            print("Error: No valid training data found", file=sys.stderr)
            sys.exit(84)

    elif args.predict:
        for line in lines:
            line = line.strip()
            if not line:
                continue
            
            parts = line.split()
            # Reconstruct FEN: take all parts except if the last one is a known label
            known_labels = ['Nothing', 'Check', 'Checkmate']
            if len(parts) > 0 and parts[-1] in [label + color for label in known_labels for color in ['', ' White', ' Black']]:
                # If there's an expected output, exclude it
                fen_parts = parts[:-1]
            else:
                fen_parts = parts
            
            fen = ' '.join(fen_parts)
            X = fen_to_vector(fen)
            output = network.predict(X)
            label = vector_to_label(output, fen)
            print(label)

if __name__ == '__main__':
    main()