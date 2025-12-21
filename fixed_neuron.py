#!/usr/bin/env python3
# fixed_neuron.py - Minimal working neural network

import numpy as np

class FixedLayer:
    def __init__(self, input_size, output_size, activation='relu'):
        self.input_size = input_size
        self.output_size = output_size
        self.activation = activation
        
        # Initialize weights
        if activation == 'relu':
            self.weights = np.random.randn(output_size, input_size) * np.sqrt(2.0 / input_size)
        elif activation in ['sigmoid', 'tanh']:
            self.weights = np.random.randn(output_size, input_size) * np.sqrt(1.0 / input_size)
        else:  # softmax
            self.weights = np.random.randn(output_size, input_size) * 0.01
        
        self.biases = np.zeros((output_size, 1))
        
        # Cache
        self.input = None
        self.output = None
        self.z = None
    
    def forward(self, X, training=True):
        self.input = X
        self.z = self.weights @ X + self.biases
        
        if self.activation == 'relu':
            self.output = np.maximum(0, self.z)
        elif self.activation == 'sigmoid':
            self.output = 1.0 / (1.0 + np.exp(-np.clip(self.z, -50, 50)))
        elif self.activation == 'softmax':
            # Stable softmax
            exp_z = np.exp(self.z - np.max(self.z, axis=0, keepdims=True))
            self.output = exp_z / np.sum(exp_z, axis=0, keepdims=True)
        else:
            self.output = self.z
        
        return self.output
    
    def backward(self, d_output, learning_rate):
        batch_size = self.input.shape[1]
        
        if self.activation == 'relu':
            d_z = d_output * (self.z > 0)
        elif self.activation == 'sigmoid':
            sig = self.output
            d_z = d_output * sig * (1 - sig)
        elif self.activation == 'softmax':
            # For softmax + cross-entropy: d_z = d_output (y_pred - y_true)
            d_z = d_output
        else:
            d_z = d_output
        
        # Gradients
        d_weights = (d_z @ self.input.T) / batch_size
        d_biases = np.sum(d_z, axis=1, keepdims=True) / batch_size
        d_input = self.weights.T @ d_z
        
        # Update
        self.weights -= learning_rate * d_weights
        self.biases -= learning_rate * d_biases
        
        return d_input

class FixedNeuron:
    def __init__(self, loss='categorical_crossentropy', learning_rate=0.01):
        self.layers = []
        self.learning_rate = learning_rate
        self.loss = loss
    
    def add_layer(self, input_size, output_size, activation='relu'):
        layer = FixedLayer(input_size, output_size, activation)
        self.layers.append(layer)
    
    def forward(self, X, training=True):
        output = X
        for layer in self.layers:
            output = layer.forward(output, training)
        return output
    
    def compute_loss(self, y_true, y_pred):
        if self.loss == 'categorical_crossentropy':
            # Add small epsilon to avoid log(0)
            epsilon = 1e-10
            y_pred = np.clip(y_pred, epsilon, 1 - epsilon)
            return -np.mean(np.sum(y_true * np.log(y_pred), axis=0))
        elif self.loss == 'binary_crossentropy':
            epsilon = 1e-10
            y_pred = np.clip(y_pred, epsilon, 1 - epsilon)
            return -np.mean(y_true * np.log(y_pred) + (1 - y_true) * np.log(1 - y_pred))
        else:
            return np.mean((y_true - y_pred) ** 2)
    
    def backward(self, y_true, y_pred):
        # For categorical cross-entropy + softmax: dL/dz = y_pred - y_true
        if self.loss == 'categorical_crossentropy':
            d_output = y_pred - y_true
        elif self.loss == 'binary_crossentropy':
            d_output = (y_pred - y_true) / (y_pred * (1 - y_pred) + 1e-10)
        else:  # MSE
            d_output = 2 * (y_pred - y_true) / y_true.shape[1]
        
        # Backpropagate
        for layer in reversed(self.layers):
            d_output = layer.backward(d_output, self.learning_rate)
    
    def train(self, X, y, epochs=100):
        for epoch in range(epochs):
            y_pred = self.forward(X, training=True)
            loss = self.compute_loss(y, y_pred)
            self.backward(y, y_pred)
            
            if epoch % 10 == 0:
                acc = np.mean(np.argmax(y_pred, axis=0) == np.argmax(y, axis=0))
                print(f"Epoch {epoch:3d}: loss={loss:.4f}, acc={acc:.2%}")
    
    def predict(self, X):
        output = self.forward(X, training=False)
        return np.argmax(output, axis=0)

# Test
def test_fixed():
    print("Testing FIXED neural network...")
    
    # XOR test
    X = np.array([[0,0],[0,1],[1,0],[1,1]]).T
    y = np.array([[1,0],[0,1],[0,1],[1,0]]).T
    
    network = FixedNeuron(loss='categorical_crossentropy', learning_rate=0.5)
    network.add_layer(2, 4, 'sigmoid')
    network.add_layer(4, 2, 'softmax')
    
    print("Training XOR...")
    network.train(X, y, epochs=1000)
    
    print("\nPredictions:")
    for i in range(4):
        X_test = X[:, i:i+1]
        pred = network.predict(X_test)
        true = np.argmax(y[:, i])
        print(f"  Input {X[:, i]}: Predicted {pred[0]}, True {true}")

if __name__ == '__main__':
    test_fixed()