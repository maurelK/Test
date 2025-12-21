import numpy as np


# ===== MSE =====
def mean_squared_error(y_true, y_pred):
    return np.mean((y_true - y_pred) ** 2)

def mean_squared_error_derivative(y_true, y_pred):
    return 2 * (y_pred - y_true) / y_true.size


# ===== Binary Cross Entropy =====
def binary_cross_entropy(y_true, y_pred, epsilon=1e-10):
    y_pred = np.clip(y_pred, epsilon, 1 - epsilon)
    return -np.mean(
        y_true * np.log(y_pred) + (1 - y_true) * np.log(1 - y_pred)
    )

def binary_cross_entropy_derivative(y_true, y_pred, epsilon=1e-10):
    y_pred = np.clip(y_pred, epsilon, 1 - epsilon)
    return (y_pred - y_true) / (y_pred * (1 - y_pred))


# ===== Categorical Cross Entropy =====
def categorical_cross_entropy(y_true, y_pred, epsilon=1e-10):
    y_pred = np.clip(y_pred, epsilon, 1 - epsilon)
    return -np.mean(np.sum(y_true * np.log(y_pred), axis=0))

def categorical_cross_entropy_derivative(y_true, y_pred):
    return y_pred - y_true  # pour softmax + cross-entropy

def define_cost_function(name):
    if name == 'mse':
        return mean_squared_error, mean_squared_error_derivative
    elif name == 'bce':
        return binary_cross_entropy, binary_cross_entropy_derivative
    elif name in ['cce', 'categorical_crossentropy']:
        return categorical_cross_entropy, categorical_cross_entropy_derivative
    else:
        raise ValueError(f"Unknown cost function: {name}")
