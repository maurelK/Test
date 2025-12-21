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
def mse_derivative(y_true, y_pred):
    return (y_pred - y_true) / y_true.shape[1]


def bce_derivative(y_true, y_pred):
    return (y_pred - y_true) / (y_pred * (1 - y_pred))


def cce_derivative(y_true, y_pred):
    return (y_pred - y_true)


# Dictionnaire des loss functions
LOSSES = {
    'mse': (mean_squared_error, mse_derivative),
    'binary_crossentropy': (binary_cross_entropy, bce_derivative),
    'categorical_crossentropy': (categorical_cross_entropy, cce_derivative),
}


def get_loss(name):
    """Retourne la loss function et sa dérivée"""
    if name not in LOSSES:
        raise ValueError(f"Unknown loss: {name}. Available: {list(LOSSES.keys())}")
    return LOSSES[name]


def define_cost_function(name):
    loss_func, deriv_func = get_loss(name)
    class CostFunction:
        def __call__(self, y_true, y_pred):
            return loss_func(y_true, y_pred)
        def derivative(self, y_true, y_pred):
            return deriv_func(y_true, y_pred)
    return CostFunction()