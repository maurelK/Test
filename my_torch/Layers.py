import numpy as np
from .activations import apply_activation, activation_derivative

class Layer:
    def __init__(self, input_size, output_size, activation='relu', initialisation='auto', dropout_rate=0.0):
        
        self.input_size = input_size
        self.output_size = output_size
        self.activation = activation
        self.dropout_rate = dropout_rate
        self.weight = self.handle_weight_initialization(initialisation)
        self.bias = np.zeros((output_size, 1))
        self.input_cache = None
        self.output_cache = None
        self.dropout_mask = None
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
        

    def forward(self, inputs, training=True):
        self.input_cache = inputs
        Z = np.dot(self.weight, inputs) + self.bias
        
        # Apply dropout during training
        if training and self.dropout_rate > 0:
            self.dropout_mask = np.random.rand(*Z.shape) > self.dropout_rate
            Z = Z * self.dropout_mask / (1 - self.dropout_rate)
        
        self.output_cache = apply_activation(Z, self.activation)
        return self.output_cache
        

    

    def backward(self, output_gradient, learning_rate, l2_lambda=0.0):
        batch_size = self.input_cache.shape[1]
        
        # IMPORTANT: Handle softmax specially
        if self.activation == 'softmax':
            # For softmax + cross-entropy, gradient is already y_pred - y_true
            z_gradient = output_gradient
        else:
            activation_gradient = activation_derivative(self.output_cache, self.activation)
            z_gradient = output_gradient * activation_gradient
    
        # Apply dropout mask if exists
        if self.dropout_mask is not None:
            z_gradient = z_gradient * self.dropout_mask / (1 - self.dropout_rate)
    
        # Compute gradients (CORRECTED)
        input_gradient = np.dot(self.weight.T, z_gradient)
        weight_gradient = np.dot(z_gradient, self.input_cache.T) / batch_size
        bias_gradient = np.sum(z_gradient, axis=1, keepdims=True) / batch_size  # FIXED: sum, not mean
    
        # L2 regularization
        if l2_lambda > 0:
            weight_gradient += l2_lambda * self.weight
    
        # Update parameters
        self.weight -= learning_rate * weight_gradient
        self.bias -= learning_rate * bias_gradient
    
        return input_gradient