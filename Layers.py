import numpy as np
from activations import apply_activation

#class Layer:
#    def __init__(self, input_size, output_size, activation='relu', initialisation='auto'):
#        
#        self.input_size = input_size
#        self.output_size = output_size
#        self.activation = activation
#        self.weight = self.handle_weight_initialization(initialisation)
#        self.bias = np.zeros((output_size, 1))
#        self.input_cache = None
#        self.output_cache = None
#
#
#    #def handle_weight_initialization(self, initialization):
#    #    if initialization == 'auto':
#    #        if self.activation == 'relu':
#    #            initialization = 'he'
#    #        elif self.activation in ['sigmoid', 'tanh', 'softmax']:
#    #            initialization = 'xavier'
#    #        else:
#    #            initialization = 'random'
##
#    #    if initialization == 'he':
#    #        return np.random.randn(self.output_size, self.input_size) * np.sqrt(2 / self.input_size)
#    #    elif initialization == 'xavier':
#    #        return np.random.randn(self.output_size, self.input_size) * np.sqrt(2 / (self.input_size + self.output_size))
#    #    elif initialization == 'random':
#    #        return np.random.randn(self.output_size, self.input_size) * 0.01
#    #    else:
#    #        raise ValueError(f"We don't know anymore: {initialization}")
#        
#
#    def _initialize_weights(self, method):
#        """Initialisation des poids selon la méthode choisie"""
#        
#        if method == 'auto':
#            # Automatique selon activation
#            if self.activation == 'relu':
#                method = 'he'
#            elif self.activation in ['sigmoid', 'tanh', 'softmax']:
#                method = 'xavier'
#            else:
#                method = 'random'
#        
#        # Calculer l'échelle
#        if method == 'he':
#            # He initialization (pour ReLU)
#            scale = np.sqrt(2.0 / self.input_size)
#        elif method == 'xavier':
#            # Xavier/Glorot (pour sigmoid, tanh)
#            scale = np.sqrt(2.0 / (self.input_size + self.output_size))
#        else:  # 'random'
#            scale = 0.01
#        
#        # SHAPE CORRECTE : (output_size, input_size)
#        return np.random.randn(self.output_size, self.input_size) * scale
#
#
#    def forward(self, inputs):
#        self.input_cache = inputs
#        Z = np.dot(self.weight, inputs) + self.bias
#        self.output_cache = self.apply_activation(Z)
#        return self.output_cache
#        
#
#    def backward(self, output_gradient, learning_rate):
#        batch_size = self.input_cache.shape[1]
#        activation_gradient = self.activation_derivative(self.output_cache)
#        z_gradient = output_gradient * activation_gradient
#
#        input_gradient = np.dot(self.weight.T, z_gradient)
#        weight_gradient = np.dot(z_gradient, self.input_cache.T) / batch_size
#        bias_gradient = np.mean(z_gradient, axis=1, keepdims=True) / self.input_cache.shape[1]
#
#        self.weight -= learning_rate * weight_gradient
#        self.bias -= learning_rate * bias_gradient
#
#        return input_gradient
#    
#


import numpy as np

class Layer:
    """Dense / Fully Connected Layer"""
    
    def __init__(self, input_size, output_size, activation='relu'):
        self.input_size = input_size
        self.output_size = output_size
        self.activation = activation
        
        # Initialisation adaptée à l'activation
        self.weights = self._init_weights()
        self.biases = np.zeros((output_size, 1))
        
        # Cache pour backward
        self.input_cache = None
        self.z_cache = None
        self.output_cache = None
        
        # Gradients
        self.grad_weights = None
        self.grad_biases = None
    
    def _init_weights(self):
        """Initialise les poids selon l'activation"""
        if self.activation == 'relu':
            # He initialization
            scale = np.sqrt(2.0 / self.input_size)
        elif self.activation in ['sigmoid', 'tanh', 'softmax']:
            # Xavier initialization
            scale = np.sqrt(2.0 / (self.input_size + self.output_size))
        else:
            scale = 0.01
        
        # SHAPE CORRECTE : (output_size, input_size)
        return np.random.randn(self.output_size, self.input_size) * scale
    
    def forward(self, X):
        """
        Forward pass
        Args:
            X: (input_size, batch_size)
        Returns:
            A: (output_size, batch_size)
        """
        self.input_cache = X
        
        # Z = W @ X + b
        self.z_cache = self.weights @ X + self.biases
        
        # Apply activation
        if self.activation == 'relu':
            self.output_cache = np.maximum(0, self.z_cache)
        elif self.activation == 'sigmoid':
            self.output_cache = 1.0 / (1.0 + np.exp(-np.clip(self.z_cache, -500, 500)))
        elif self.activation == 'tanh':
            self.output_cache = np.tanh(self.z_cache)
        elif self.activation == 'softmax':
            exp_z = np.exp(self.z_cache - np.max(self.z_cache, axis=0, keepdims=True))
            self.output_cache = exp_z / np.sum(exp_z, axis=0, keepdims=True)
        else:  # linear
            self.output_cache = self.z_cache
        
        return self.output_cache
    
    def backward(self, grad_output, learning_rate):
        """
        Backward pass with parameter update
        Args:
            grad_output: gradient from next layer
            learning_rate: learning rate
        Returns:
            grad_input: gradient to propagate to previous layer
        """
        batch_size = self.input_cache.shape[1]
        
        # Gradient w.r.t Z (apply activation derivative)
        if self.activation == 'relu':
            grad_z = grad_output * (self.z_cache > 0).astype(float)
        elif self.activation == 'sigmoid':
            sig = self.output_cache
            grad_z = grad_output * sig * (1 - sig)
        elif self.activation == 'tanh':
            grad_z = grad_output * (1 - self.output_cache ** 2)
        elif self.activation == 'softmax':
            # Simplified for softmax + cross-entropy
            grad_z = grad_output
        else:  # linear
            grad_z = grad_output
        
        # Gradients of parameters
        self.grad_weights = (grad_z @ self.input_cache.T) / batch_size
        self.grad_biases = np.sum(grad_z, axis=1, keepdims=True) / batch_size
        
        # Update parameters (gradient descent)
        self.weights -= learning_rate * self.grad_weights
        self.biases -= learning_rate * self.grad_biases
        
        # Gradient to propagate
        grad_input = self.weights.T @ grad_z
        
        return grad_input
    
    def get_params(self):
        """Return parameters for saving"""
        return {
            'weights': self.weights,
            'biases': self.biases,
            'input_size': self.input_size,
            'output_size': self.output_size,
            'activation': self.activation
        }
    
    def set_params(self, params):
        """Load parameters"""
        self.weights = params['weights']
        self.biases = params['biases']
        self.input_size = params['input_size']
        self.output_size = params['output_size']
        self.activation = params['activation']