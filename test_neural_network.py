#!/usr/bin/env python3

import numpy as np
import sys
import os
sys.path.append(os.path.dirname(__file__))

from my_torch.Neuron import Neuron

def create_dummy_data():
    """Create simple dummy data for testing the neural network"""
    # XOR problem: 4 samples, 2 inputs, 1 output
    X = np.array([
        [0, 0],
        [0, 1],
        [1, 0],
        [1, 1]
    ]).T  # Shape: (2, 4)

    y = np.array([
        [1, 0],  # Nothing (0 XOR 0 = 0)
        [0, 1],  # Check (0 XOR 1 = 1)
        [0, 1],  # Check (1 XOR 0 = 1)
        [1, 0]   # Nothing (1 XOR 1 = 0)
    ]).T  # Shape: (2, 4) for binary classification

    return X, y

def test_neural_network():
    """Test the neural network with dummy data"""
    print("Creating neural network...")
    network = Neuron(loss='binary_crossentropy', learning_rate=0.1)

    # Add layers: 2 inputs -> 4 hidden -> 2 outputs
    network.add_layer(2, 4, 'relu')
    network.add_layer(4, 2, 'sigmoid')

    print("Generating dummy data...")
    X, y = create_dummy_data()

    print("Training network...")
    network.train(X, y, epochs=1000)

    print("\nTesting predictions...")
    predictions = network.predict(X)
    print("Predictions:")
    print(predictions.T)

    print("\nExpected:")
    print(y.T)

    # Calculate accuracy
    pred_classes = np.argmax(predictions, axis=0)
    true_classes = np.argmax(y, axis=0)
    accuracy = np.mean(pred_classes == true_classes)
    print(f"\nAccuracy: {accuracy * 100:.2f}%")

    return network

if __name__ == '__main__':
    trained_network = test_neural_network()

    # Save the trained network
    trained_network.save('my_torch_network_test.nn')
    print("\nNetwork saved as 'my_torch_network_test.nn'")