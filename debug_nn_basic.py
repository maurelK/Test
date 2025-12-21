#!/usr/bin/env python3
# debug_nn_basic.py

import numpy as np
import sys
import os
sys.path.append(os.path.dirname(__file__))

from my_torch.Neuron import Neuron

def test_xor():
    """Test with XOR problem - should be learnable"""
    print("Testing XOR problem (should get ~100% accuracy)...")
    
    # XOR data
    X = np.array([
        [0, 0],
        [0, 1],
        [1, 0],
        [1, 1]
    ]).T  # (2, 4)
    
    y = np.array([
        [1, 0],  # 0 -> Nothing
        [0, 1],  # 1 -> Check
        [0, 1],  # 1 -> Check
        [1, 0]   # 0 -> Nothing
    ]).T  # (2, 4)
    
    print(f"X shape: {X.shape}, y shape: {y.shape}")
    
    # Simple network
    network = Neuron(loss='binary_crossentropy', learning_rate=0.1)
    network.add_layer(2, 4, 'sigmoid')
    network.add_layer(4, 2, 'sigmoid')
    
    # Train
    print("\nTraining XOR...")
    for epoch in range(1000):
        y_pred = network.forward(X, training=True)
        loss = network.cost_function(y, y_pred)
        network.backward(y, y_pred)
        
        if epoch % 100 == 0:
            acc = np.mean(np.argmax(y_pred, axis=0) == np.argmax(y, axis=0))
            print(f"Epoch {epoch:3d}: loss={loss:.4f}, acc={acc:.2%}")
    
    # Test
    print("\nFinal predictions:")
    for i in range(4):
        X_test = X[:, i:i+1]
        output = network.forward(X_test, training=False)
        pred = np.argmax(output)
        true = np.argmax(y[:, i])
        print(f"  Input {X[:, i]}, Output: {output.flatten()}, Pred: {pred}, True: {true}")

def test_simple_linear():
    """Test with simple linear relationship"""
    print("\n" + "="*50)
    print("Testing simple linear relationship...")
    
    # y = x1 > 0.5
    X = np.random.rand(2, 100)
    y = np.zeros((2, 100))
    y[0, :] = X[0, :] <= 0.5  # Class 0 if x1 <= 0.5
    y[1, :] = X[0, :] > 0.5   # Class 1 if x1 > 0.5
    
    network = Neuron(loss='binary_crossentropy', learning_rate=0.1)
    network.add_layer(2, 4, 'sigmoid')
    network.add_layer(4, 2, 'sigmoid')
    
    print("Training...")
    for epoch in range(100):
        y_pred = network.forward(X, training=True)
        loss = network.cost_function(y, y_pred)
        network.backward(y, y_pred)
        
        if epoch % 20 == 0:
            acc = np.mean(np.argmax(y_pred, axis=0) == np.argmax(y, axis=0))
            print(f"Epoch {epoch:3d}: loss={loss:.4f}, acc={acc:.2%}")
    
    print("\nShould be >70% accuracy")

def inspect_network():
    """Inspect network weights and gradients"""
    print("\n" + "="*50)
    print("Inspecting network...")
    
    network = Neuron(loss='categorical_crossentropy', learning_rate=0.01)
    network.add_layer(12, 24, 'relu')
    network.add_layer(24, 3, 'softmax')
    
    # Create dummy data
    X = np.random.randn(12, 10)
    y = np.eye(3)[np.random.choice(3, 10)].T
    
    print("Before training:")
    for i, layer in enumerate(network.layers):
        print(f"  Layer {i} weights mean: {np.mean(layer.weight):.6f}, std: {np.std(layer.weight):.6f}")
        print(f"  Layer {i} biases: {layer.bias.flatten()}")
    
    # One training step
    print("\nOne training step...")
    y_pred = network.forward(X, training=True)
    loss_before = network.cost_function(y, y_pred)
    network.backward(y, y_pred)
    
    y_pred_after = network.forward(X, training=True)
    loss_after = network.cost_function(y, y_pred_after)
    
    print(f"Loss before: {loss_before:.6f}")
    print(f"Loss after:  {loss_after:.6f}")
    print(f"Difference:  {loss_after - loss_before:.6f} (should be negative)")
    
    print("\nAfter training:")
    for i, layer in enumerate(network.layers):
        print(f"  Layer {i} weights mean: {np.mean(layer.weight):.6f}")

if __name__ == '__main__':
    test_xor()
    test_simple_linear()
    inspect_network()