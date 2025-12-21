#!/usr/bin/env python3
# train_with_features.py

import numpy as np
import sys
import os
sys.path.append(os.path.dirname(__file__))

from my_torch.Neuron import Neuron
from chess_features_enhanced import extract_chess_features

print("Training with ENHANCED chess features...")

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
        
        features = extract_chess_features(fen).flatten()
        X_list.append(features)
        y_list.append(label_vec)

X = np.array(X_list).T
y = np.array(y_list).T

print(f"Loaded {X.shape[1]} samples")
print(f"Input size: {X.shape[0]} features (was 768)")
print(f"Output size: {y.shape[0]}")

# Check class distribution
print(f"\nClass distribution:")
class_counts = np.sum(y, axis=1)
for i, count in enumerate(class_counts):
    label = ['Nothing', 'Check', 'Checkmate'][i]
    print(f"  {label}: {int(count)} samples ({count/X.shape[1]:.1%})")

# Create network
input_size = X.shape[0]
network = Neuron(loss='categorical_crossentropy', learning_rate=0.01)

# Smaller network for 45 features
network.add_layer(input_size, 64, 'relu')
network.add_layer(64, 32, 'relu')
network.add_layer(32, 16, 'relu')
network.add_layer(16, 3, 'softmax')

print(f"\nNetwork architecture: {input_size} -> 64 -> 32 -> 16 -> 3")

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

# Train
best_val_acc = 0
patience = 30

for epoch in range(1000):
    # Train
    y_pred_train = network.forward(X_train, training=True)
    loss_train = network.cost_function(y_train, y_pred_train)
    network.backward(y_train, y_pred_train)
    
    # Validate
    if epoch % 20 == 0:
        y_pred_val = network.forward(X_val, training=False)
        loss_val = network.cost_function(y_val, y_pred_val)
        
        train_acc = np.mean(np.argmax(y_pred_train, axis=0) == np.argmax(y_train, axis=0))
        val_acc = np.mean(np.argmax(y_pred_val, axis=0) == np.argmax(y_val, axis=0))
        
        if val_acc > best_val_acc:
            best_val_acc = val_acc
            patience_counter = 0
            network.save('best_features.nn')
            print(f"Epoch {epoch:3d}: Train Loss={loss_train:.4f} Acc={train_acc:.2%} | "
                  f"Val Loss={loss_val:.4f} Acc={val_acc:.2%} *")
        else:
            patience_counter += 1
            if epoch % 100 == 0:
                print(f"Epoch {epoch:3d}: Train Loss={loss_train:.4f} Acc={train_acc:.2%} | "
                      f"Val Loss={loss_val:.4f} Acc={val_acc:.2%}")
    
    if patience_counter >= patience:
        print(f"\nEarly stopping at epoch {epoch}")
        break

# Load best
network = Neuron.load('best_features.nn')
network.save('my_torch_network_features.nn')

print(f"\nBest validation accuracy: {best_val_acc:.2%}")
print("Saved as 'my_torch_network_features.nn'")

# Test
print("\nTesting on sample positions:")
test_cases = [
    ("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", "Nothing"),
    ("rnb1kbnr/pppp1ppp/8/4p3/6Pq/5P2/PPPPP2P/RNBQKBNR w KQkq - 1 3", "Check"),
    ("8/8/8/8/8/8/8/k1K5 w - - 0 1", "Checkmate"),
]

for fen, expected in test_cases:
    features = extract_chess_features(fen)
    output = network.forward(features, training=False)
    pred_idx = np.argmax(output)
    
    classes = ['Nothing', 'Check', 'Checkmate']
    pred_label = classes[pred_idx]
    
    print(f"  {fen[:40]}...")
    print(f"    -> Predicted: {pred_label}")
    print(f"    -> Expected: {expected}")
    print(f"    -> Confidence: {output.flatten()}")
    print()