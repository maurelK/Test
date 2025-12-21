#!/usr/bin/env python3
import numpy as np
import sys
import os
sys.path.append(os.path.dirname(__file__))

from my_torch.Neuron import Neuron

def fen_to_vector_correct(fen):
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

def test_encoding():
    """Test the encoding with known positions"""
    print("Testing FEN encoding...")
    
    # Starting position
    fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
    vec = fen_to_vector_correct(fen)
    
    print(f"Starting position:")
    print(f"  Non-zero elements: {np.sum(vec != 0)}")
    print(f"  Should be 32 pieces")
    
    # Check each piece type
    piece_map = {'P':0,'N':1,'B':2,'R':3,'Q':4,'K':5,'p':6,'n':7,'b':8,'r':9,'q':10,'k':11}
    
    for piece, channel in piece_map.items():
        start = channel * 64
        end = start + 64
        count = np.sum(vec[start:end] != 0)
        if count > 0:
            print(f"  {piece}: {count}")
    
    # Test an endgame position
    fen2 = "8/8/R2k4/4r1p1/8/5K2/5P2/8 b - - 7 59"
    vec2 = fen_to_vector_correct(fen2)
    print(f"\nEndgame position:")
    print(f"  Non-zero elements: {np.sum(vec2 != 0)}")
    
    return vec, vec2

def load_and_train_correct():
    print("\n" + "="*50)
    print("Loading dataset with correct encoding...")
    
    X = []
    y = []
    
    with open('balanced_dataset.txt', 'r') as f:
        for line in f:
            line = line.strip()
            if not line:
                continue
            
            parts = line.split()
            if len(parts) < 7:
                continue
            
            # Parse label
            if parts[-1] == 'Nothing':
                fen = ' '.join(parts[:-1])
                label_idx = 0  # Nothing
            elif 'Checkmate' in parts[-2]:
                fen = ' '.join(parts[:-2])
                label_idx = 2  # Checkmate
            elif 'Check' in parts[-2]:
                fen = ' '.join(parts[:-2])
                label_idx = 1  # Check
            else:
                continue
            
            # Convert with correct encoding
            vec = fen_to_vector_correct(fen).flatten()
            X.append(vec)
            
            # One-hot encoding
            one_hot = [0, 0, 0]
            one_hot[label_idx] = 1
            y.append(one_hot)
    
    X = np.array(X).T
    y = np.array(y).T
    
    print(f"Dataset loaded: {X.shape[1]} samples")
    print(f"Average non-zero per sample: {np.mean(np.sum(X != 0, axis=0)):.1f}")
    
    # Create and train network
    print("\nCreating network...")
    network = Neuron(
        loss='categorical_crossentropy',
        learning_rate=0.001,
        l2_lambda=0.0001
    )
    
    network.add_layer(768, 256, 'relu')
    network.add_layer(256, 128, 'relu')
    network.add_layer(128, 64, 'relu')
    network.add_layer(64, 3, 'softmax')
    
    # Split
    n_samples = X.shape[1]
    n_train = int(0.8 * n_samples)
    
    indices = np.random.permutation(n_samples)
    train_idx = indices[:n_train]
    val_idx = indices[n_train:]
    
    X_train = X[:, train_idx]
    y_train = y[:, train_idx]
    X_val = X[:, val_idx]
    y_val = y[:, val_idx]
    
    print(f"Training on {X_train.shape[1]} samples, validating on {X_val.shape[1]}")
    
    # Train
    best_val_acc = 0
    patience = 20
    
    for epoch in range(200):
        # Train
        y_pred_train = network.forward(X_train, training=True)
        loss_train = network.cost_function(y_train, y_pred_train)
        network.backward(y_train, y_pred_train)
        
        # Validate
        y_pred_val = network.forward(X_val, training=False)
        loss_val = network.cost_function(y_val, y_pred_val)
        
        train_acc = np.mean(np.argmax(y_pred_train, axis=0) == np.argmax(y_train, axis=0))
        val_acc = np.mean(np.argmax(y_pred_val, axis=0) == np.argmax(y_val, axis=0))
        
        if val_acc > best_val_acc:
            best_val_acc = val_acc
            patience_counter = 0
            network.save('best_correct.nn')
        else:
            patience_counter += 1
        
        if epoch % 5 == 0:
            print(f"Epoch {epoch:3d}: Train Loss={loss_train:.4f} Acc={train_acc:.2%} | "
                  f"Val Loss={loss_val:.4f} Acc={val_acc:.2%}")
        
        if patience_counter >= patience:
            print(f"\nEarly stopping at epoch {epoch}")
            break
    
    # Load best
    network = Neuron.load('best_correct.nn')
    network.save('my_torch_network_correct.nn')
    
    print(f"\nBest validation accuracy: {best_val_acc:.2%}")
    
    return network

if __name__ == '__main__':
    # Test encoding first
    vec1, vec2 = test_encoding()
    
    # Train
    network = load_and_train_correct()
    
    # Quick test
    print("\n" + "="*50)
    print("Quick test on sample positions:")
    
    test_cases = [
        ("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", "Nothing"),
        ("rnb1kbnr/pppp1ppp/8/4p3/6Pq/5P2/PPPPP2P/RNBQKBNR w KQkq - 1 3", "Check"),
        ("8/8/8/8/8/8/8/k1K5 w - - 0 1", "Checkmate"),
    ]
    
    for fen, expected in test_cases:
        X_test = fen_to_vector_correct(fen)
        output = network.forward(X_test, training=False)
        pred_idx = np.argmax(output)
        
        classes = ['Nothing', 'Check', 'Checkmate']
        prediction = classes[pred_idx]
        
        print(f"\nFEN: {fen[:40]}...")
        print(f"  Expected: {expected}")
        print(f"  Predicted: {prediction}")
        print(f"  Output: {output.flatten()}")