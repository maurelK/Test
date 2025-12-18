import numpy as np
import json
class Perceptron:
    def __init__(self, learning_rate, numb_inputs):
        self.weight = np.random.rand(numb_inputs + 1)
        self.learning_rate = learning_rate

    def linear(self, input):
        Z = input @ self.weight[1:].T + self.weight[0]
        return Z
    
    def heaviside(self, Z):
        if Z >= 0:
            return 1
        else :
            return 0
        
    
    def prediction(self, inputs):
        Z = self.linear(inputs)
        if isinstance(Z, np.ndarray):
            return np.array([self.heaviside(z) for z in Z])
        else:
            return self.heaviside(Z)
    

    def log_loss(self, predi, tahg):
        loss = (predi -tahg)
        return loss
    
    def train(self, inputs, target):
        predite = self.prediction(inputs)
        loss = self.log_loss(predite, target)
        self.weight[1:] += self.learning_rate * loss*inputs
        self.weight[0] += self.learning_rate * loss

    def iterate(self, numb_epochs, X, y):
        for epoch in range(numb_epochs):
            for inputs , target in zip(X, y):
                self.train(inputs, target)

    
    def save_state(self, filename):
        state = {
            'learning_rate' :self.learning_rate,
            'weights': self.weight.tolist()
        }
        with open(filename, 'w') as f:
            json.dump(state, f, indent=2)

    def load_file(self, filename):
        with open(filename, 'r') as f:
            state = json.load(f)