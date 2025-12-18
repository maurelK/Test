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
    elif activation == 'linear':
        return z
    else:
        raise ValueError(f"Unknown activation function: {activation}")

def sigmoid_derivative(a):
    return a * (1 - a)

def tanh_derivative(a):
    return 1 - a**2

def relu_derivative(a):
    return (a > 0).astype(float)

def softmax_derivative(a):
    # For softmax, derivative is complex, but for backprop, it's handled in the loss
    # For now, return identity as it's usually combined with cross-entropy
    return np.ones_like(a)

def activation_derivative(a, activation):
    if activation == 'sigmoid':
        return sigmoid_derivative(a)
    elif activation == 'tanh':
        return tanh_derivative(a)
    elif activation == 'relu':
        return relu_derivative(a)
    elif activation == 'softmax':
        return softmax_derivative(a)
    elif activation == 'linear':
        return np.ones_like(a)
    else:
        raise ValueError(f"Unknown activation function: {activation}")