#!/usr/bin/env python3
# train_trivial.py

import numpy as np
import sys
import os
sys.path.append(os.path.dirname(__file__))

from my_torch.Neuron import Neuron

def fen_to_vector_simple(fen):
    """Super simple encoding: just count pieces"""
    board_part = fen.split()[0]
    
    # Just 12 features: counts of each piece type
    piece_counts = np.zeros(12)
    
    piece_map = {
        'P': 0, 'N': 1, 'B': 2, 'R': 3, 'Q': 4, 'K': 5,
        'p': 6, 'n': 7, 'b': 8, 'r': 9, 'q': 10, 'k': 11
    }
    
    for char in board_part:
        if char.isdigit() or char == '/':
            continue
        if char in piece_map:
            piece_counts[piece_map[char]] += 1
    
    return piece_counts.reshape(-1, 1)

def load_trivial_data():
    X = []
    y = []
    
    # For trivial dataset, we only have 3 classes
    label_map = {
        'Nothing': 0,
        'Checkmate White': 1,
        'Checkmate Black': 2
    }
    
    with open('trivial_dataset.txt', 'r') as f:
        for line in f:
            line = line.strip()
            if not line:
                continue
            
            parts = line.split()
            fen = ' '.join(parts[:-1])
            label = ' '.join(parts[-2:]) if 'Checkmate' in line else parts[-1]
            
            if label not in label_map:
                print(f"Skipping unknown label: {label}")
                continue
            
            vec = fen_to_vector_simple(fen).flatten()
            X.append(vec)
            
            idx = label_map[label]
            one_hot = [0, 0, 0]  # 3 classes
            one_hot[idx] = 1
            y.append(one_hot)
    
    return np.array(X).T, np.array(y).T

def main():
    print("Training on TRIVIAL dataset (piece counts only)...")
    
    X, y = load_trivial_data()
    print(f"Data shape: X={X.shape}, y={y.shape}")
    print(f"Input features: just 12 piece counts")
    
    # Check class distribution
    class_counts = np.sum(y, axis=1)
    print(f"Class distribution: {class_counts}")
    
    # Very simple network
    network = Neuron(loss='categorical_crossentropy', learning_rate=0.01)
    network.add_layer(12, 24, 'relu')
    network.add_layer(24, 12, 'relu')
    network.add_layer(12, 3, 'softmax')
    
    # Train
    print("\nTraining...")
    for epoch in range(100):
        y_pred = network.forward(X, training=True)
        loss = network.cost_function(y, y_pred)
        network.backward(y, y_pred)
        
        if epoch % 10 == 0:
            acc = np.mean(np.argmax(y_pred, axis=0) == np.argmax(y, axis=0))
            print(f"Epoch {epoch}: loss={loss:.4f}, acc={acc:.2%}")
    
    # Save
    network.save('my_torch_network_trivial.nn')
    print("\nSaved as 'my_torch_network_trivial.nn'")
    
    # Test
    print("\nTesting rules:")
    test_cases = [
        ("8/8/8/8/8/8/8/K7 w - - 0 1", "Checkmate Black"),
        ("k7/8/8/8/8/8/8/8 w - - 0 1", "Checkmate White"),
        ("Kk6/8/8/8/8/8/8/8 w - - 0 1", "Nothing"),
        ("KQk5/8/8/8/8/8/8/8 w - - 0 1", "Checkmate White"),
    ]
    
    labels = ['Nothing', 'Checkmate White', 'Checkmate Black']
    
    for fen, expected in test_cases:
        X_test = fen_to_vector_simple(fen)
        output = network.forward(X_test, training=False)
        pred_idx = np.argmax(output)
        pred_label = labels[pred_idx]
        
        print(f"  {fen:30} -> {pred_label:20} (expected: {expected})")
        print(f"    Output: {output.flatten()}")

if __name__ == '__main__':
    main()