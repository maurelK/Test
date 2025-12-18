import numpy as np
from activations import apply_activation

class Layer:
    def __init__(self, input_size, output_size, activation='he', initialisation='auto'):
        
        self.input_size = input_size
        self.output_size = output_size
        self.activation = activation
        self.weight = self.handle_weight_initialization(initialisation)
        self.bias = np.zeros((output_size, 1))
        self.input_cache = None
        self.output_cache = None


    def handle_weight_initialization(self, initialization):
        if initialization == 'auto':
            if self.activation == 'relu':
                initialization = 'he'
            elif self.activation in ['sigmoid', 'tanh', 'softmax']:
                initialization = 'xavier'
            else:
                initialization = 'random'

        if initialization == 'he':
            return np.random.randn(self.output_size, self.input_size) * np.sqrt(2 / self.input_size)
        elif initialization == 'xavier':
            return np.random.randn(self.output_size, self.input_size) * np.sqrt(1 / self.input_size)
        elif initialization == 'random':
            return np.random.randn(self.output_size, self.input_size) * 0.01
        else:
            raise ValueError(f"We don't know anymore: {initialization}")
        

    def forward(self, inputs):
        self.input_cache = inputs
        Z = np.dot(self.weight, inputs) + self.bias
        self.output_cache = self.apply_activation(Z)
        return self.output_cache
        

    def backward(self, output_gradient, learning_rate):
        batch_size = self.input_cache.shape[1]
        activation_gradient = self.activation_derivative(self.output_cache)
        z_gradient = output_gradient * activation_gradient

        input_gradient = np.dot(self.weight.T, z_gradient)
        weight_gradient = np.dot(z_gradient, self.input_cache.T) / batch_size
        bias_gradient = np.mean(z_gradient, axis=1, keepdims=True) / self.input_cache.shape[1]

        self.weight -= learning_rate * weight_gradient
        self.bias -= learning_rate * bias_gradient

        return input_gradient
    
