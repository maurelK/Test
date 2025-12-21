#!/usr/bin/env python3
# test_fixed_implementation.py

import numpy as np
import sys
import os
sys.path.append(os.path.dirname(__file__))

# First, let's verify the cost function fix
print("Testing cost function derivatives...")

# Import your fixed cost functions
from my_torch.cost_functions import cce_derivative

# Test case
y_true = np.array([[1, 0, 0], [0, 1, 0]]).T  # (3, 2)
y_pred = np.array([[0.7, 0.2, 0.1], [0.3, 0.4, 0.3]]).T  # (3, 2)

grad = cce_derivative(y_true, y_pred)
print(f"y_true shape: {y_true.shape}")
print(f"y_pred shape: {y_pred.shape}")
print(f"Gradient shape: {grad.shape}")
print(f"Gradient:\n{grad}")

# Check: gradient = y_pred - y_true
expected = y_pred - y_true
print(f"\nExpected (y_pred - y_true):\n{expected}")
print(f"\nMax difference: {np.max(np.abs(grad - expected)):.2e}")

if np.allclose(grad, expected):
    print("✓ Cost function derivative is CORRECT!")
else:
    print("✗ Cost function derivative is WRONG!")

# Now test the full network
print("\n" + "="*50)
print("Testing YOUR fixed network implementation...")

from my_torch.Neuron import Neuron

# Create network with YOUR implementation
network = Neuron(loss='categorical_crossentropy', learning_rate=0.5)  # Higher LR for XOR
network.add_layer(2, 4, 'sigmoid')
network.add_layer(4, 2, 'softmax')

# XOR data
X = np.array([[0,0],[0,1],[1,0],[1,1]]).T
y = np.array([[1,0],[0,1],[0,1],[1,0]]).T

print(f"X shape: {X.shape}, y shape: {y.shape}")

# Train
print("\nTraining XOR with YOUR implementation...")
for epoch in range(1000):
    y_pred = network.forward(X, training=True)
    loss = network.cost_function(y, y_pred)
    network.backward(y, y_pred)
    
    if epoch % 100 == 0:
        acc = np.mean(np.argmax(y_pred, axis=0) == np.argmax(y, axis=0))
        print(f"Epoch {epoch:3d}: loss={loss:.4f}, acc={acc:.2%}")

# Test
print("\nPredictions:")
for i in range(4):
    X_test = X[:, i:i+1]
    output = network.forward(X_test, training=False)
    pred = np.argmax(output)
    true = np.argmax(y[:, i])
    print(f"  Input {X[:, i]}: Output {output.flatten()}, Pred {pred}, True {true}")

# Save
network.save('my_torch_network_fixed_test.nn')
print("\nSaved as 'my_torch_network_fixed_test.nn'")