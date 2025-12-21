from .cost_functions import define_cost_function
from .Layers import Layer
import numpy as np
import json

class Neuron:
    def __init__(self, loss='categorical_crossentropy', learning_rate=0.01, l2_lambda=0.0, dropout_rates=None):
        self.layers = []
        self.learning_rate = learning_rate
        self.l2_lambda = l2_lambda
        self.dropout_rates = dropout_rates or []
        self.cost_function = define_cost_function(loss)
        self.loss_name = loss

    def add_layer(self, input_size, output_size, activation, initialisation='auto', dropout_rate=0.0):
        layer = Layer(input_size, output_size, activation, initialisation, dropout_rate)
        self.layers.append(layer)
    
    def forward(self, X, training=True):
        output = X
        for layer in self.layers:
            output = layer.forward(output, training)
        return output

    def backward(self, y_true, y_pred):
        loss_grad = self.cost_function.derivative(y_true, y_pred)
        for layer in reversed(self.layers):
            loss_grad = layer.backward(loss_grad, self.learning_rate, self.l2_lambda)

    #def train(self, X_train, y_train, X_val=None, y_val=None, 
    #          epochs=100, batch_size=32, verbose=True):
    #    """Train with mini-batches and validation"""
    #    
    #    n_samples = X_train.shape[1]
    #    n_batches = n_samples // batch_size
    #    
    #    history = {
    #        'train_loss': [],
    #        'train_accuracy': [],
    #        'val_loss': [],
    #        'val_accuracy': []
    #    }
    #    
    #    for epoch in range(epochs):
    #        # Shuffle
    #        indices = np.random.permutation(n_samples)
    #        X_shuffled = X_train[:, indices]
    #        y_shuffled = y_train[:, indices]
    #        
    #        epoch_loss = 0
    #        correct = 0
    #        
    #        # Mini-batches
    #        for batch in range(n_batches):
    #            start = batch * batch_size
    #            end = start + batch_size
    #            X_batch = X_shuffled[:, start:end]
    #            y_batch = y_shuffled[:, start:end]
    #            
    #            # Train
    #            y_pred = self.forward(X_batch)
    #            batch_loss = self.cost_function(y_batch, y_pred)
    #            epoch_loss += batch_loss
    #            
    #            # Accuracy
    #            predictions = np.argmax(y_pred, axis=0)
    #            labels = np.argmax(y_batch, axis=0)
    #            correct += np.sum(predictions == labels)
    #            
    #            self.backward(y_batch, y_pred)
    #        
    #        # Metrics
    #        avg_loss = epoch_loss / n_batches
    #        train_accuracy = correct / n_samples
    #        
    #        history['train_loss'].append(avg_loss)
    #        history['train_accuracy'].append(train_accuracy)
    #        
    #        # Validation
    #        if X_val is not None and y_val is not None:
    #            val_loss, val_accuracy = self.evaluate(X_val, y_val)
    #            history['val_loss'].append(val_loss)
    #            history['val_accuracy'].append(val_accuracy)
    #            
    #            if verbose and epoch % 10 == 0:
    #                print(f"Epoch {epoch}/{epochs}")
    #                print(f"  Train - Loss: {avg_loss:.4f}, Acc: {train_accuracy:.2%}")
    #                print(f"  Val   - Loss: {val_loss:.4f}, Acc: {val_accuracy:.2%}")
    #
    #    return history


    def train(self, X, y, epochs=100):
        """Simplified training for now - fix basic issues first"""
        n_samples = X.shape[1]
        
        for epoch in range(epochs):
            # Forward pass
            y_pred = self.forward(X, training=True)
            
            # Calculate loss
            loss = self.cost_function(y, y_pred)
            
            # Calculate accuracy
            predictions = np.argmax(y_pred, axis=0)
            labels = np.argmax(y, axis=0)
            accuracy = np.mean(predictions == labels)
            
            # Backward pass
            self.backward(y, y_pred)
            
            if epoch % 10 == 0:
                print(f"Epoch {epoch}: Loss = {loss:.4f}, Accuracy = {accuracy:.2%}")
        
        return loss

    def evaluate(self, X, y):
        """Evaluate on dataset"""
        y_pred = self.forward(X, training=False)
        loss = self.cost_function(y, y_pred)
        
        predictions = np.argmax(y_pred, axis=0)
        labels = np.argmax(y, axis=0)
        accuracy = np.mean(predictions == labels)
        
        return loss, accuracy

    def predict(self, X):
        """Return class indices (0, 1, or 2) - inference mode without dropout"""
        y_pred = self.forward(X, training=False)
        predictions = np.argmax(y_pred, axis=0)
        
        if predictions.shape[0] == 1:
            return predictions[0]  # Scalar for single sample
        
        return predictions


    def save(self, filename):
        network_data = {
            'learning_rate': self.learning_rate,
            'l2_lambda': self.l2_lambda,
            'dropout_rates': self.dropout_rates,
            'loss': self.loss_name,
            'layers': []
        }
        for layer in self.layers:
            layer_data = {
                'input_size': layer.input_size,
                'output_size': layer.output_size,
                'activation': layer.activation,
                'dropout_rate': layer.dropout_rate,
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
        
        # Handle backward compatibility for older saved networks
        l2_lambda = network_data.get('l2_lambda', 0.0)
        dropout_rates = network_data.get('dropout_rates', [])
        loss = network_data.get('loss', 'categorical_crossentropy')
        learning_rate = network_data.get('learning_rate', 0.01)
        
        network = cls(loss=loss, learning_rate=learning_rate, l2_lambda=l2_lambda, dropout_rates=dropout_rates)
        
        for layer_data in network_data['layers']:
            layer = Layer(
                layer_data['input_size'], 
                layer_data['output_size'], 
                layer_data['activation'],
                dropout_rate=layer_data.get('dropout_rate', 0.0)
            )
            layer.weight = np.array(layer_data['weights'])
            layer.bias = np.array(layer_data['bias'])
            network.layers.append(layer)
        return network
