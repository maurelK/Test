#!/usr/bin/env python3
import numpy as np
from my_torch.Neuron import Neuron

# Simple test
X = np.random.randn(768, 100)
y = np.random.choice([0, 1], size=100)
y_onehot = np.zeros((2, 100))
y_onehot[y, np.arange(100)] = 1

network = Neuron(loss='categorical_crossentropy', learning_rate=0.01)
network.add_layer(768, 64, 'relu')
network.add_layer(64, 32, 'relu')
network.add_layer(32, 2, 'softmax')

print("Training...")
network.train(X, y_onehot, epochs=50)

# Save
network.save('test_network.nn')
print("Saved test_network.nn")