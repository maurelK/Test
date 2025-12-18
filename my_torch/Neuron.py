from .cost_functions import define_cost_function
from .Layers import Layer
import numpy as np
import json

class Neuron:
    def __init__(self, loss='categorical_crossentropy'):
        self.layers = []
        self.learning_rate = learning_rate
        self.cost_function = define_cost_function(loss)
        self.loss_name = loss

    def add_layer(self, input_size, output_size, activation, initialisation='auto'):
        layer = Layer(input_size, output_size, activation, initialisation)
        self.layers.append(layer)
    
    def forward(self, X):
        output = X
        for layer in self.layers:
            output = layer.forward(output)
        return output

    def backward(self, y_true, y_pred):
        loss_grad = self.cost_function.derivative(y_true, y_pred)
        for layer in reversed(self.layers):
            loss_grad = layer.backward(loss_grad, self.learning_rate)

    #def train(self, X, y, epochs=100):
    #    for epoch in range(epochs):
    #        y_pred = self.forward(X)
    #        self.backward(y, y_pred)
    #        if epoch % 10 == 0:
    #            loss = self.cost_function(y, y_pred)
    #            print(f"Epoch {epoch}, Loss: {loss}")

        def train(self, X_train, y_train, X_val=None, y_val=None, 
              epochs=100, batch_size=32, verbose=True):
        """Train with mini-batches and validation"""
        
        n_samples = X_train.shape[1]
        n_batches = n_samples // batch_size
        
        history = {
            'train_loss': [],
            'train_accuracy': [],
            'val_loss': [],
            'val_accuracy': []
        }
        
        for epoch in range(epochs):
            # Shuffle
            indices = np.random.permutation(n_samples)
            X_shuffled = X_train[:, indices]
            y_shuffled = y_train[:, indices]
            
            epoch_loss = 0
            correct = 0
            
            # Mini-batches
            for batch in range(n_batches):
                start = batch * batch_size
                end = start + batch_size
                X_batch = X_shuffled[:, start:end]
                y_batch = y_shuffled[:, start:end]
                
                # Train
                y_pred = self.forward(X_batch)
                batch_loss = self.cost_function(y_batch, y_pred)
                epoch_loss += batch_loss
                
                # Accuracy
                predictions = np.argmax(y_pred, axis=0)
                labels = np.argmax(y_batch, axis=0)
                correct += np.sum(predictions == labels)
                
                self.backward(y_batch, y_pred)
            
            # Metrics
            avg_loss = epoch_loss / n_batches
            train_accuracy = correct / n_samples
            
            history['train_loss'].append(avg_loss)
            history['train_accuracy'].append(train_accuracy)
            
            # Validation
            if X_val is not None and y_val is not None:
                val_loss, val_accuracy = self.evaluate(X_val, y_val)
                history['val_loss'].append(val_loss)
                history['val_accuracy'].append(val_accuracy)
                
                if verbose and epoch % 10 == 0:
                    print(f"Epoch {epoch}/{epochs}")
                    print(f"  Train - Loss: {avg_loss:.4f}, Acc: {train_accuracy:.2%}")
                    print(f"  Val   - Loss: {val_loss:.4f}, Acc: {val_accuracy:.2%}")
        
        return history

    
    def evaluate(self, X, y):
        """Evaluate on dataset"""
        y_pred = self.forward(X)
        loss = self.cost_function(y, y_pred)
        
        predictions = np.argmax(y_pred, axis=0)
        labels = np.argmax(y, axis=0)
        accuracy = np.mean(predictions == labels)
        
        return loss, accuracy

    #def predict(self, X):
    #    return self.forward(X)

    def predict(self, X):
        """Return class indices (0, 1, or 2)"""
        y_pred = self.forward(X)
        predictions = np.argmax(y_pred, axis=0)
        
        if predictions.shape[0] == 1:
            return predictions[0]  # Scalar for single sample
        
        return predictions


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
