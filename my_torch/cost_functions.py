import numpy as np


def mean_squared_error(y_true, y_pred):
    return np.mean((y_true - y_pred) ** 2)


def binary_cross_entropy(y_true, y_pred, epsilon=1e-10):
    y_pred = np.clip(y_pred, epsilon, 1 - epsilon)# Clip pour éviter log(0)
    
    loss = -np.mean(
        y_true * np.log(y_pred) + (1 - y_true) * np.log(1 - y_pred)
    )
    return loss

def categorical_cross_entropy(y_true, y_pred, epsilon=1e-10):
    y_pred = np.clip(y_pred, epsilon, 1 - epsilon)# Clip pour éviter log(0)
    
    loss = -np.mean(np.sum(y_true * np.log(y_pred), axis=0))
    return loss



def define_cost_function(name):
    if name == 'mse':
        return mean_squared_error
    elif name == 'bce':
        return binary_cross_entropy
    elif name in ['cce', 'categorical_crossentropy']:
        return categorical_cross_entropy
    else:
        raise ValueError(f"Unknown cost function: {name}")