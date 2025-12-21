#!/usr/bin/env python3
# test_xor_fixed.py

import numpy as np
import sys
import os
sys.path.append(os.path.dirname(__file__))

from my_torch.Neuron import Neuron

print("Testing XOR with FIXED implementation...")

# XOR data
X = np.array([[0,0],[0,1],[1,0],[1,1]]).T
y = np.array([[1,0],[0,1],[0,1],[1,0]]).T

print(f"X shape: {X.shape}, y shape: {y.shape}")

# Try different architectures
architectures = [
    ("Small", [(2, 4, 'sigmoid'), (4, 2, 'softmax')]),
    ("Medium", [(2, 8, 'sigmoid'), (8, 4, 'sigmoid'), (4, 2, 'softmax')]),
    ("Large", [(2, 16, 'sigmoid'), (16, 8, 'sigmoid'), (8, 4, 'sigmoid'), (4, 2, 'softmax')]),
]

for name, layers in architectures:
    print(f"\n{'='*50}")
    print(f"Testing {name} architecture...")
    
    network = Neuron(loss='categorical_crossentropy', learning_rate=0.5)
    for input_size, output_size, activation in layers:
        network.add_layer(input_size, output_size, activation)
    
    # Train
    best_acc = 0
    for epoch in range(2000):
        y_pred = network.forward(X, training=True)
        loss = network.cost_function(y, y_pred)
        network.backward(y, y_pred)
        
        if epoch % 200 == 0:
            acc = np.mean(np.argmax(y_pred, axis=0) == np.argmax(y, axis=0))
            best_acc = max(best_acc, acc)
            if epoch % 400 == 0:
                print(f"Epoch {epoch:4d}: loss={loss:.4f}, acc={acc:.2%}")
    
    # Final test
    y_pred_final = network.forward(X, training=False)
    final_acc = np.mean(np.argmax(y_pred_final, axis=0) == np.argmax(y, axis=0))
    
    print(f"Final accuracy: {final_acc:.2%}")
    print(f"Best accuracy: {best_acc:.2%}")
    
    # Show predictions
    print("Predictions:")
    for i in range(4):
        output = network.forward(X[:, i:i+1], training=False)
        pred = np.argmax(output)
        true = np.argmax(y[:, i])
        print(f"  Input {X[:, i]}: Output {output.flatten()}, Pred {pred}, True {true}")
    
    if final_acc == 1.0:
        print(f"✓ {name} architecture solved XOR!")
        network.save(f'my_torch_network_xor_{name}.nn')
        break