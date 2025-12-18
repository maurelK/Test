from .cost_functions import define_cost_function
from .Layers import Layer
import numpy as np
import json

class Neuron:
    def __init__(self, loss='categorical_crossentropy', learning_rate=0.01):
        self.layers = []
        self.learning_rate = learning_rate
        self.cost_function = define_cost_function(loss)

    def add_layer(self, input_size, output_size, activation, initialisation='auto'):
        layer = Layer(input_size, output_size, activation, initialisation)
        self.layers.append(layer)
    
    def forward(self, X):
        output = X
        for layer in self.layers:
            output = layer.forward(output)
        return output

    def backward(self, y_true, y_pred):
        # Compute loss gradient
        loss_grad = self.cost_function.derivative(y_true, y_pred)
        # Backpropagate through layers
        for layer in reversed(self.layers):
            loss_grad = layer.backward(loss_grad, self.learning_rate)

    def train(self, X, y, epochs=100):
        for epoch in range(epochs):
            y_pred = self.forward(X)
            self.backward(y, y_pred)
            if epoch % 10 == 0:
                loss = self.cost_function(y, y_pred)
                print(f"Epoch {epoch}, Loss: {loss}")

    def predict(self, X):
        return self.forward(X)

    def save(self, filename):
        network_data = {
            'learning_rate': self.learning_rate,
            'loss': 'categorical_crossentropy',  # hardcoded for now
            'layers': []
        }
        for layer in self.layers:
            layer_data = {
                'input_size': layer.input_size,
                'output_size': layer.output_size,
                'activation': layer.activation,
                'weights': layer.weight.tolist(),
                'bias': layer.bias.tolist()
            }
            network_data['layers'].append(layer_data)
        with open(filename, 'w') as f:
            json.dump(network_data, f, indent=4)

    @classmethod
    def load(cls, filename):
        with open(filename, 'r') as f:
            network_data = json.load(f)
        network = cls(loss=network_data['loss'], learning_rate=network_data['learning_rate'])
        for layer_data in network_data['layers']:
            layer = Layer(layer_data['input_size'], layer_data['output_size'], layer_data['activation'])
            layer.weight = np.array(layer_data['weights'])
            layer.bias = np.array(layer_data['bias'])
            network.layers.append(layer)
        return network
