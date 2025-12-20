#!/usr/bin/env python3

import numpy as np
import sys
import os
sys.path.append(os.path.dirname(__file__))

from my_torch.Neuron import Neuron

# Mapping des labels vers indices
LABEL_TO_INDEX = {
    'Nothing': 0,
    'Check': 1,
    'Checkmate': 2
}

INDEX_TO_LABEL = {v: k for k, v in LABEL_TO_INDEX.items()}

def load_chess_dataset(filename):
    """Charge le dataset d'échecs depuis un fichier"""
    X = []
    y = []

    with open(filename, 'r') as f:
        for line_num, line in enumerate(f, 1):
            line = line.strip()
            if not line:
                continue

            parts = line.split()
            if len(parts) < 7:  # FEN complet + au moins 1 label
                print(f"Ligne {line_num} ignorée: pas assez de parties ({len(parts)})")
                continue

            # Gestion spéciale pour les fichiers nothing qui n'ont qu'un label
            if len(parts) == 7:  # FEN + Nothing
                fen = ' '.join(parts[:-1])
                label_full = parts[-1]
            else:  # FEN + label + couleur
                fen = ' '.join(parts[:-2])
                label_full = ' '.join(parts[-2:])

            # Extraire le type de label (Check, Checkmate, Nothing)
            if 'Checkmate' in label_full:
                label = 'Checkmate'
            elif 'Check' in label_full:
                label = 'Check'
            else:
                label = 'Nothing'

            # Convertir FEN en vecteur one-hot
            vector = fen_to_vector(fen)
            X.append(vector.flatten())

            # Convertir label en one-hot
            label_idx = LABEL_TO_INDEX[label]
            one_hot = np.zeros(3)
            one_hot[label_idx] = 1
            y.append(one_hot)

    X = np.array(X).T  # Shape: (768, n_samples)
    y = np.array(y).T  # Shape: (3, n_samples)

    print(f"Dataset chargé: {X.shape[1]} échantillons")
    print(f"Distribution des classes:")
    for label, idx in LABEL_TO_INDEX.items():
        count = np.sum(y[idx, :])
        print(f"  {label}: {int(count)} échantillons")

    return X, y

def fen_to_vector(fen):
    """Convertit une chaîne FEN en vecteur one-hot 768 éléments (12 canaux × 64 cases)"""
    board_part = fen.split()[0]  # Prendre seulement la partie plateau
    
    # Mapping des pièces vers les canaux (0-5: blanc, 6-11: noir)
    piece_map = {
        'P': 0, 'N': 1, 'B': 2, 'R': 3, 'Q': 4, 'K': 5,      # Pièces blanches
        'p': 6, 'n': 7, 'b': 8, 'r': 9, 'q': 10, 'k': 11     # Pièces noires
    }
    
    vector = np.zeros(768)  # 12 canaux × 64 cases
    square_idx = 0
    
    for char in board_part:
        if char.isdigit():
            # Sauter les cases vides
            square_idx += int(char)
        elif char == '/':
            # Sauter les séparateurs de rangées
            continue
        elif char in piece_map:
            # Activer le canal correspondant pour cette pièce
            channel = piece_map[char]
            vector[channel * 64 + square_idx] = 1
            square_idx += 1
    
    return vector.reshape(-1, 1)

def create_chess_network():
    """Crée un réseau de neurones optimisé pour l'analyse d'échecs"""
    print("Création du réseau d'analyse d'échecs optimisé...")

    network = Neuron(
        loss='categorical_crossentropy',
        learning_rate=0.001,  # Learning rate réduit pour stabilité
        l2_lambda=0.0001      # Régularisation L2 légère
    )

    # Architecture optimisée avec plus de neurones et dropout progressif
    network.add_layer(768, 512, 'relu', dropout_rate=0.3)    # Entrée one-hot -> Couche cachée 1
    network.add_layer(512, 256, 'relu', dropout_rate=0.25)   # Couche cachée 1 -> Couche cachée 2
    network.add_layer(256, 128, 'relu', dropout_rate=0.2)    # Couche cachée 2 -> Couche cachée 3
    network.add_layer(128, 64, 'relu', dropout_rate=0.1)     # Couche cachée 3 -> Couche cachée 4
    network.add_layer(64, 3, 'softmax', dropout_rate=0.0)    # Couche cachée 4 -> Sortie

    print("Architecture optimisée du réseau:")
    print("  Entrée: 768 neurones (encodage one-hot FEN)")
    print("  Couche cachée 1: 512 neurones (ReLU + Dropout 30%)")
    print("  Couche cachée 2: 256 neurones (ReLU + Dropout 25%)")
    print("  Couche cachée 3: 128 neurones (ReLU + Dropout 20%)")
    print("  Couche cachée 4: 64 neurones (ReLU + Dropout 10%)")
    print("  Sortie: 3 neurones (Softmax)")
    print("  Régularisation: L2 λ=0.0001, Learning rate=0.001")

    return network

def evaluate_network(network, X, y):
    """Évalue les performances du réseau"""
    predictions = network.predict(X)
    pred_classes = np.argmax(predictions, axis=0)
    true_classes = np.argmax(y, axis=0)

    accuracy = np.mean(pred_classes == true_classes)
    print(".2f")

    # Matrice de confusion simple
    print("Matrice de confusion:")
    for true_idx in range(3):
        for pred_idx in range(3):
            count = np.sum((true_classes == true_idx) & (pred_classes == pred_idx))
            true_label = INDEX_TO_LABEL[true_idx]
            pred_label = INDEX_TO_LABEL[pred_idx]
            print(f"  {true_label} -> {pred_label}: {int(count)}")

    return accuracy

def train_chess_network_enhanced(dataset_file, epochs=200, batch_size=64, patience=10, 
                               save_path='my_torch_network_enhanced.nn'):
    """Entraîne le réseau avec améliorations : early stopping, validation, learning rate decay"""
    print("=== Entraînement amélioré du réseau d'analyse d'échecs ===")

    # Charger les données
    X, y = load_chess_dataset(dataset_file)
    
    # Split train/validation (80/20)
    n_samples = X.shape[1]
    n_train = int(0.8 * n_samples)
    
    indices = np.random.permutation(n_samples)
    train_indices = indices[:n_train]
    val_indices = indices[n_train:]
    
    X_train = X[:, train_indices]
    y_train = y[:, train_indices]
    X_val = X[:, val_indices]
    y_val = y[:, val_indices]
    
    print(f"Split: {n_train} train, {n_samples - n_train} validation samples")

    # Créer le réseau
    network = create_chess_network()

    print(f"\nDébut de l'entraînement sur {X_train.shape[1]} échantillons...")
    print(f"Nombre d'époques max: {epochs}, Patience: {patience}, Batch size: {batch_size}")

    # Variables pour early stopping
    best_val_accuracy = 0
    patience_counter = 0
    best_weights = None
    
    # Learning rate decay
    initial_lr = network.learning_rate
    decay_rate = 0.95
    decay_steps = 20

    for epoch in range(epochs):
        # Learning rate decay
        if epoch > 0 and epoch % decay_steps == 0:
            network.learning_rate *= decay_rate
            print(".6f")

        # Entraînement
        history = network.train(X_train, y_train, X_val, y_val, epochs=1, batch_size=batch_size, verbose=False)
        
        train_loss = history['train_loss'][0]
        train_acc = history['train_accuracy'][0]
        val_loss = history['val_loss'][0] 
        val_acc = history['val_accuracy'][0]
        
        print(f"Époque {epoch + 1:2d} - Train: Loss={train_loss:.4f}, Acc={train_acc:.1f}% | Val: Loss={val_loss:.4f}, Acc={val_acc:.1f}%")
        # Early stopping
        if val_acc > best_val_accuracy:
            best_val_accuracy = val_acc
            patience_counter = 0
            best_weights = [layer.weight.copy() for layer in network.layers]
            print(f"  → Nouveau meilleur modèle (validation: {best_val_accuracy:.1f}%)")
        else:
            patience_counter += 1
            
        if patience_counter >= patience:
            print(f"\n🛑 Early stopping après {epoch + 1} époques (patience épuisée)")
            break
    
    # Restaurer les meilleurs poids
    if best_weights:
        for i, layer in enumerate(network.layers):
            layer.weight = best_weights[i]
        print("✅ Meilleurs poids restaurés")
    
    print("\nÉvaluation finale sur validation...")
    final_val_loss, final_val_accuracy = network.evaluate(X_val, y_val)
    print(f"Précision finale sur validation: {final_val_accuracy:.2f}%")
    # Sauvegarder le modèle entraîné
    network.save(save_path)
    print(f"\n💾 Modèle sauvegardé: {save_path}")

    return network, final_val_accuracy

if __name__ == '__main__':
    import argparse

    parser = argparse.ArgumentParser(description='Entraîner un réseau MY_TORCH sur des données d\'échecs')
    parser.add_argument('dataset', help='Fichier contenant les données d\'entraînement (FEN + labels)')
    parser.add_argument('--epochs', type=int, default=200, help='Nombre d\'époques d\'entraînement maximum')
    parser.add_argument('--batch-size', type=int, default=64, help='Taille des mini-batches')
    parser.add_argument('--patience', type=int, default=10, help='Patience pour early stopping')
    parser.add_argument('--save', default='my_torch_network_enhanced.nn', help='Chemin de sauvegarde du modèle')

    args = parser.parse_args()

    try:
        network, accuracy = train_chess_network_enhanced(
            args.dataset, 
            epochs=args.epochs,
            batch_size=args.batch_size,
            patience=args.patience,
            save_path=args.save
        )
        print(f"\n🎯 Précision finale sur validation: {accuracy:.2f}")
    except Exception as e:
        print(f"Erreur lors de l'entraînement: {e}", file=sys.stderr)
        sys.exit(84)