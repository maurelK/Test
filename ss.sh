#!/usr/bin/env python3
import numpy as np
import json

# Load and inspect the network
with open('my_torch_network_basic_1.nn', 'r') as f:
    network_data = json.load(f)

print("Network structure:")
print(f"Learning rate: {network_data['learning_rate']}")
print(f"Loss: {network_data['loss']}")
print(f"Number of layers: {len(network_data['layers'])}")

for i, layer in enumerate(network_data['layers']):
    print(f"\nLayer {i}:")
    print(f"  Input size: {layer['input_size']}")
    print(f"  Output size: {layer['output_size']}")
    print(f"  Activation: {layer['activation']}")
    
    weights = np.array(layer['weights'])
    biases = np.array(layer['bias'])
    
    print(f"  Weight shape: {weights.shape}")
    print(f"  Bias shape: {biases.shape}")
    print(f"  Weight mean: {np.mean(weights):.6f}")
    print(f"  Weight std: {np.std(weights):.6f}")
    print(f"  Bias mean: {np.mean(biases):.6f}")
    
    # Check last layer specifically
    if i == len(network_data['layers']) - 1:
        print(f"  Last layer biases: {biases.flatten()}")