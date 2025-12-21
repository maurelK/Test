#!/usr/bin/env python3
# debug_backprop.py

import numpy as np
import sys
import os
sys.path.append(os.path.dirname(__file__))

from my_torch.Layers import Layer

def test_layer_gradients():
    """Test if layer computes gradients correctly"""
    print("Testing layer gradient computation...")
    
    # Create a simple layer
    layer = Layer(2, 3, activation='sigmoid')
    
    # Input
    X = np.array([[0.5, 0.3, 0.8], [0.2, 0.7, 0.4]]).T  # (2, 3) but we need (2, batch)
    X = X.T  # Actually need (input_size, batch_size) = (2, 3)
    
    print(f"Input shape: {X.shape}")
    
    # Forward pass
    output = layer.forward(X, training=True)
    print(f"Output shape: {output.shape}")
    print(f"Output:\n{output}")
    
    # Mock gradient from next layer
    d_output = np.random.randn(3, 3) * 0.1  # (output_size, batch_size)
    
    print(f"\nd_output (gradient from next layer):\n{d_output}")
    
    # Backward pass
    learning_rate = 0.1
    d_input = layer.backward(d_output, learning_rate)
    
    print(f"\nd_input (gradient to previous layer) shape: {d_input.shape}")
    print(f"d_input:\n{d_input}")
    
    # Check if weights changed
    print(f"\nWeight change norm: {np.linalg.norm(layer.weight):.6f}")
    print(f"Bias change norm: {np.linalg.norm(layer.bias):.6f}")

def test_gradient_flow():
    """Test gradient flow through network"""
    print("\n" + "="*50)
    print("Testing gradient flow...")
    
    from my_torch.Neuron import Neuron
    
    # Tiny network
    network = Neuron(loss='categorical_crossentropy', learning_rate=0.5)
    network.add_layer(2, 2, 'sigmoid')
    network.add_layer(2, 2, 'softmax')
    
    # Single sample
    X = np.array([[0.5], [0.3]])  # (2, 1)
    y = np.array([[1], [0]])  # (2, 1)
    
    print(f"X: {X.flatten()}")
    print(f"y: {y.flatten()}")
    
    # Save initial weights
    initial_weights = [layer.weight.copy() for layer in network.layers]
    
    # Forward
    y_pred = network.forward(X, training=True)
    print(f"\ny_pred: {y_pred.flatten()}")
    
    # Compute loss gradient manually
    loss_grad = y_pred - y  # For softmax + cross-entropy
    print(f"Loss gradient (y_pred - y): {loss_grad.flatten()}")
    
    # One backward pass
    network.backward(y, y_pred)
    
    # Check weight changes
    print(f"\nWeight changes:")
    for i, (layer, init_w) in enumerate(zip(network.layers, initial_weights)):
        change = np.linalg.norm(layer.weight - init_w)
        print(f"  Layer {i}: {change:.6f}")
        
        if change < 1e-10:
            print(f"    ⚠️  WARNING: Layer {i} weights didn't change!")
    
    # Forward again to check if loss decreases
    y_pred2 = network.forward(X, training=True)
    loss1 = network.cost_function(y, y_pred)
    loss2 = network.cost_function(y, y_pred2)
    
    print(f"\nLoss before: {loss1:.6f}")
    print(f"Loss after:  {loss2:.6f}")
    print(f"Change:      {loss2 - loss1:.6f} (should be negative)")

if __name__ == '__main__':
    test_layer_gradients()
    test_gradient_flow()