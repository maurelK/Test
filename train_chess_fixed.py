#!/usr/bin/env python3
# train_chess_fixed.py

import numpy as np
import sys
import os
sys.path.append(os.path.dirname(__file__))

from my_torch.Neuron import Neuron

def fen_to_vector(fen):
    """Your working FEN encoding"""
    board_part = fen.split()[0]
    piece_map = {
        'P': 0, 'N': 1, 'B': 2, 'R': 3, 'Q': 4, 'K': 5,
        'p': 6, 'n': 7, 'b': 8, 'r': 9, 'q': 10, 'k': 11
    }
    vector = np.zeros(768)
    square_idx = 0
    for char in board_part:
        if char.isdigit():
            square_idx += int(char)
        elif char == '/':
            continue
        elif char in piece_map:
            channel = piece_map[char]
            vector[channel * 64 + square_idx] = 1
            square_idx += 1
    return vector.reshape(-1, 1)

print("Training on chess data with FIXED implementation...")

# Load data
X_list = []
y_list = []

with open('balanced_dataset.txt', 'r') as f:
    for line in f:
        line = line.strip()
        if not line:
            continue
        
        parts = line.split()
        if len(parts) < 7:
            continue
        
        # Parse
        if parts[-1] == 'Nothing':
            fen = ' '.join(parts[:-1])
            label_vec = [1, 0, 0]  # Nothing
        elif 'Checkmate' in parts[-2]:
            fen = ' '.join(parts[:-2])
            label_vec = [0, 0, 1]  # Checkmate
        elif 'Check' in parts[-2]:
            fen = ' '.join(parts[:-2])
            label_vec = [0, 1, 0]  # Check
        else:
            continue
        
        X_list.append(fen_to_vector(fen).flatten())
        y_list.append(label_vec)

X = np.array(X_list).T
y = np.array(y_list).T

print(f"Loaded {X.shape[1]} samples")
print(f"Input size: {X.shape[0]}, Output size: {y.shape[0]}")

# Create network
network = Neuron(loss='categorical_crossentropy', learning_rate=0.001)
network.add_layer(768, 256, 'relu', dropout_rate=0.3)
network.add_layer(256, 128, 'relu', dropout_rate=0.2)
network.add_layer(128, 64, 'relu')
network.add_layer(64, 3, 'softmax')

print("\nTraining...")

# Split data
n_samples = X.shape[1]
n_train = int(0.8 * n_samples)

indices = np.random.permutation(n_samples)
train_idx = indices[:n_train]
val_idx = indices[n_train:]

X_train = X[:, train_idx]
y_train = y[:, train_idx]
X_val = X[:, val_idx]
y_val = y[:, val_idx]

print(f"Train: {X_train.shape[1]}, Validation: {X_val.shape[1]}")

# Train with early stopping
best_val_acc = 0
patience = 20
patience_counter = 0

for epoch in range(500):
    # Train
    y_pred_train = network.forward(X_train, training=True)
    loss_train = network.cost_function(y_train, y_pred_train)
    network.backward(y_train, y_pred_train)
    
    # Validate
    if epoch % 10 == 0:
        y_pred_val = network.forward(X_val, training=False)
        loss_val = network.cost_function(y_val, y_pred_val)
        
        train_acc = np.mean(np.argmax(y_pred_train, axis=0) == np.argmax(y_train, axis=0))
        val_acc = np.mean(np.argmax(y_pred_val, axis=0) == np.argmax(y_val, axis=0))
        
        if val_acc > best_val_acc:
            best_val_acc = val_acc
            patience_counter = 0
            network.save('best_chess_fixed.nn')
            print(f"Epoch {epoch:3d}: Train Loss={loss_train:.4f} Acc={train_acc:.2%} | "
                  f"Val Loss={loss_val:.4f} Acc={val_acc:.2%} *")
        else:
            patience_counter += 1
            if epoch % 50 == 0:
                print(f"Epoch {epoch:3d}: Train Loss={loss_train:.4f} Acc={train_acc:.2%} | "
                      f"Val Loss={loss_val:.4f} Acc={val_acc:.2%}")
    
    if patience_counter >= patience:
        print(f"\nEarly stopping at epoch {epoch}")
        break

# Load best
network = Neuron.load('best_chess_fixed.nn')
network.save('my_torch_network_chess_fixed.nn')

print(f"\nBest validation accuracy: {best_val_acc:.2%}")
print("Saved as 'my_torch_network_chess_fixed.nn'")

# Test
print("\nTesting on sample positions:")
test_cases = [
    ("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", "Nothing"),
    ("rnb1kbnr/pppp1ppp/8/4p3/6Pq/5P2/PPPPP2P/RNBQKBNR w KQkq - 1 3", "Check"),
    ("8/8/8/8/8/8/8/k1K5 w - - 0 1", "Checkmate"),
]

for fen, expected in test_cases:
    X_test = fen_to_vector(fen)
    output = network.forward(X_test, training=False)
    pred_idx = np.argmax(output)
    
    classes = ['Nothing', 'Check', 'Checkmate']
    print(f"  {fen[:40]}... -> {classes[pred_idx]} (expected: {expected})")