#!/usr/bin/env python3
import numpy as np
import sys
import os
sys.path.append(os.path.dirname(__file__))

from my_torch.Neuron import Neuron

def fen_to_vector(fen):
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

def analyze_dataset(filename):
    print(f"Analyzing {filename}...")
    
    X_list = []
    y_list = []
    class_counts = {'Nothing': 0, 'Check': 0, 'Checkmate': 0}
    
    with open(filename, 'r') as f:
        for line in f:
            line = line.strip()
            if not line:
                continue
            
            parts = line.split()
            if len(parts) < 2:
                continue
            
            # Parse label
            if parts[-1] == 'Nothing':
                fen = ' '.join(parts[:-1])
                label = 'Nothing'
                label_vec = [1, 0, 0]
                class_counts['Nothing'] += 1
            elif 'Checkmate' in parts[-2]:
                fen = ' '.join(parts[:-2])
                label = 'Checkmate'
                label_vec = [0, 0, 1]
                class_counts['Checkmate'] += 1
            elif 'Check' in parts[-2]:
                fen = ' '.join(parts[:-2])
                label = 'Check'
                label_vec = [0, 1, 0]
                class_counts['Check'] += 1
            else:
                continue
            
            # Convert FEN
            vec = fen_to_vector(fen).flatten()
            X_list.append(vec)
            y_list.append(label_vec)
    
    X = np.array(X_list).T
    y = np.array(y_list).T
    
    print(f"\nDataset statistics:")
    print(f"  Total samples: {X.shape[1]}")
    print(f"  Input dimension: {X.shape[0]}")
    print(f"  Class distribution:")
    for cls, count in class_counts.items():
        percentage = count / X.shape[1] * 100
        print(f"    {cls}: {count} ({percentage:.1f}%)")
    
    # Check if vectors are reasonable
    print(f"\nVector statistics:")
    print(f"  Average non-zero elements: {np.mean(np.sum(X != 0, axis=0)):.1f}")
    print(f"  Min non-zero: {np.min(np.sum(X != 0, axis=0))}")
    print(f"  Max non-zero: {np.max(np.sum(X != 0, axis=0))}")
    
    return X, y, class_counts

def train_and_evaluate(X, y, network_name='debug_network.nn'):
    print("\n" + "="*50)
    print("Training network...")
    
    # Create network with better hyperparameters
    network = Neuron(
        loss='categorical_crossentropy',
        learning_rate=0.001,  # Lower learning rate
        l2_lambda=0.0001      # Regularization
    )
    
    # Better architecture
    network.add_layer(768, 256, 'relu', dropout_rate=0.3)
    network.add_layer(256, 128, 'relu', dropout_rate=0.2)
    network.add_layer(128, 64, 'relu', dropout_rate=0.1)
    network.add_layer(64, 3, 'softmax')
    
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
    
    print(f"Train samples: {X_train.shape[1]}, Val samples: {X_val.shape[1]}")
    
    # Train with monitoring
    best_val_acc = 0
    patience = 20
    patience_counter = 0
    
    for epoch in range(200):
        # Train on training set
        y_pred_train = network.forward(X_train, training=True)
        loss_train = network.cost_function(y_train, y_pred_train)
        network.backward(y_train, y_pred_train)
        
        # Evaluate on validation set
        y_pred_val = network.forward(X_val, training=False)
        loss_val = network.cost_function(y_val, y_pred_val)
        
        # Calculate accuracies
        train_acc = np.mean(np.argmax(y_pred_train, axis=0) == np.argmax(y_train, axis=0))
        val_acc = np.mean(np.argmax(y_pred_val, axis=0) == np.argmax(y_val, axis=0))
        
        # Early stopping
        if val_acc > best_val_acc:
            best_val_acc = val_acc
            patience_counter = 0
            network.save('best_network.nn')
        else:
            patience_counter += 1
        
        if epoch % 10 == 0:
            print(f"Epoch {epoch:3d}: Train Loss={loss_train:.4f} Acc={train_acc:.2%} | "
                  f"Val Loss={loss_val:.4f} Acc={val_acc:.2%}")
        
        if patience_counter >= patience:
            print(f"\nEarly stopping at epoch {epoch}")
            break
    
    # Load best network
    network = Neuron.load('best_network.nn')
    
    # Final evaluation
    y_pred_final = network.forward(X_val, training=False)
    final_acc = np.mean(np.argmax(y_pred_final, axis=0) == np.argmax(y_val, axis=0))
    
    print(f"\nFinal validation accuracy: {final_acc:.2%}")
    
    # Save final network
    network.save(network_name)
    print(f"Network saved to {network_name}")
    
    return network, final_acc

if __name__ == '__main__':
    if len(sys.argv) > 1:
        filename = sys.argv[1]
    else:
        filename = 'balanced_dataset.txt'
    
    if not os.path.exists(filename):
        print(f"Error: File {filename} not found!")
        sys.exit(1)
    
    X, y, class_counts = analyze_dataset(filename)
    
    # Check if dataset is too small
    if X.shape[1] < 100:
        print(f"\n⚠️  WARNING: Only {X.shape[1]} samples. Neural networks need more data!")
        print("Consider generating more chess positions.")
    
    # Train
    network, accuracy = train_and_evaluate(X, y, 'my_torch_network_trained_debug.nn')
    
    # Quick test
    print("\n" + "="*50)
    print("Testing on a few samples:")
    
    with open('test_small.txt', 'r') as f:
        lines = f.readlines()[:5]
    
    for i, line in enumerate(lines):
        line = line.strip()
        if not line:
            continue
        
        parts = line.split()
        if len(parts) < 2:
            continue
        
        # Extract FEN
        if parts[-1] == 'Nothing':
            fen = ' '.join(parts[:-1])
        elif 'Check' in parts[-2] or 'Checkmate' in parts[-2]:
            fen = ' '.join(parts[:-2])
        else:
            fen = ' '.join(parts)
        
        X_test = fen_to_vector(fen)
        output = network.forward(X_test, training=False)
        pred_class = np.argmax(output)
        
        class_names = ['Nothing', 'Check', 'Checkmate']
        print(f"  Sample {i}: {class_names[pred_class]}")