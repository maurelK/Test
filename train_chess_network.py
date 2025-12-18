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

            # Convertir FEN en vecteur
            vector = fen_to_vector(fen)
            X.append(vector.flatten())

            # Convertir label en one-hot
            label_idx = LABEL_TO_INDEX[label]
            one_hot = np.zeros(3)
            one_hot[label_idx] = 1
            y.append(one_hot)

    X = np.array(X).T  # Shape: (64, n_samples)
    y = np.array(y).T  # Shape: (3, n_samples)

    print(f"Dataset chargé: {X.shape[1]} échantillons")
    print(f"Distribution des classes:")
    for label, idx in LABEL_TO_INDEX.items():
        count = np.sum(y[idx, :])
        print(f"  {label}: {int(count)} échantillons")

    return X, y

def fen_to_vector(fen):
    """Convertit une chaîne FEN en vecteur 64 éléments"""
    board_part = fen.split()[0]  # Prendre seulement la partie plateau

    vector = []
    for char in board_part:
        if char.isdigit():
            # Ajouter des zéros pour les cases vides
            vector.extend([0] * int(char))
        elif char == '/':
            # Ignorer les séparateurs de rangées
            continue
        elif char in 'PNBRQKpnbrqk':
            # Pièces : mapping simple
            piece_map = {
                'P': 1, 'N': 2, 'B': 3, 'R': 4, 'Q': 5, 'K': 6,
                'p': -1, 'n': -2, 'b': -3, 'r': -4, 'q': -5, 'k': -6
            }
            vector.append(piece_map[char])

    # S'assurer qu'on a exactement 64 éléments
    while len(vector) < 64:
        vector.append(0)
    vector = vector[:64]

    return np.array(vector).reshape(-1, 1)

def create_chess_network():
    """Crée un réseau de neurones adapté à l'analyse d'échecs"""
    print("Création du réseau d'analyse d'échecs...")

    network = Neuron(
        loss='categorical_crossentropy',
        learning_rate=0.001  # Réduit pour une convergence plus stable
    )

    # Architecture optimisée pour l'analyse d'échecs
    network.add_layer(64, 128, 'relu')    # Entrée -> Couche cachée 1
    network.add_layer(128, 64, 'relu')   # Couche cachée 1 -> Couche cachée 2
    network.add_layer(64, 32, 'relu')    # Couche cachée 2 -> Couche cachée 3
    network.add_layer(32, 3, 'softmax')  # Couche cachée 3 -> Sortie (3 classes)

    print("Architecture du réseau:")
    print("  Entrée: 64 neurones (plateau d'échecs)")
    print("  Couche cachée 1: 128 neurones (ReLU)")
    print("  Couche cachée 2: 64 neurones (ReLU)")
    print("  Couche cachée 3: 32 neurones (ReLU)")
    print("  Sortie: 3 neurones (Softmax - Nothing/Check/Checkmate)")

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
            if count > 0:
                true_label = INDEX_TO_LABEL[true_idx]
                pred_label = INDEX_TO_LABEL[pred_idx]
                print(f"  {true_label} -> {pred_label}: {int(count)}")

    return accuracy

def train_chess_network(dataset_file, epochs=1000, save_path='my_torch_network_trained.nn'):
    """Entraîne le réseau sur le dataset d'échecs"""
    print("=== Entraînement du réseau d'analyse d'échecs ===")

    # Charger les données
    X, y = load_chess_dataset(dataset_file)

    # Créer le réseau
    network = create_chess_network()

    print(f"\nDébut de l'entraînement sur {X.shape[1]} échantillons...")
    print(f"Nombre d'époques: {epochs}")

    # Entraînement
    for epoch in range(epochs):
        # Forward pass
        y_pred = network.predict(X)

        # Backward pass
        network.backward(y, y_pred)

        # Afficher la progression
        if (epoch + 1) % 100 == 0:
            loss = network.cost_function(y, y_pred)
            print(".4f")

    print("\nÉvaluation finale du modèle...")
    accuracy = evaluate_network(network, X, y)

    # Sauvegarder le modèle entraîné
    network.save(save_path)
    print(f"\nModèle sauvegardé: {save_path}")

    return network, accuracy

if __name__ == '__main__':
    import argparse

    parser = argparse.ArgumentParser(description='Entraîner un réseau MY_TORCH sur des données d\'échecs')
    parser.add_argument('dataset', help='Fichier contenant les données d\'entraînement (FEN + labels)')
    parser.add_argument('--epochs', type=int, default=1000, help='Nombre d\'époques d\'entraînement')
    parser.add_argument('--save', default='my_torch_network_trained.nn', help='Chemin de sauvegarde du modèle')

    args = parser.parse_args()

    try:
        network, accuracy = train_chess_network(args.dataset, args.epochs, args.save)
        print(f"\nPrécision finale: {accuracy:.2f}")
    except Exception as e:
        print(f"Erreur lors de l'entraînement: {e}", file=sys.stderr)
        sys.exit(84)