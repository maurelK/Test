import numpy as np


def sigmoid(z):
    return 1.0 / (1.0 + np.exp(-z))

def relu(z):
    return np.maximum(0, z)

def tanh(z):
    return np.tanh(z)

def softmax(z):
    exp_z = np.exp(z - np.max(z, axis=0, keepdims=True))
    return exp_z / np.sum(exp_z, axis=0, keepdims=True)

def apply_activation(z, activation):
    if activation == 'sigmoid':
        return sigmoid(z)
    elif activation == 'relu':
        return relu(z)
    elif activation == 'tanh':
        return tanh(z)
    elif activation == 'softmax':
        return softmax(z)
    elif activation == 'elem':
        return z
    else:
        raise ValueError(f"Unknown activation function: {activation}")

def sigmoid_derivative(z):
    s = sigmoid(z)
    return s * (1 - s)

def tanh_derivative(z):
    t = np.tanh(z)
    return 1 - t**2

def relu_derivative(z):
    return (z > 0).astype(float)

def activation_derivative(z, activation):
    if activation == 'sigmoid':
        return sigmoid_derivative(z)
    elif activation == 'tanh':
        return tanh_derivative(z)
    elif activation == 'relu':
        return relu_derivative(z)
    elif activation == 'softmax':
        # For softmax, derivative is complex, but for backprop, it's handled in the loss
        # For now, return identity as it's usually combined with cross-entropy
        return np.ones_like(z)
    elif activation == 'elem':
        return np.ones_like(z)
    else:
        raise ValueError(f"Unknown activation function: {activation}")