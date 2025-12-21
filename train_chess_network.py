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
    """Crée un réseau de neurones adapté à l'analyse d'échecs avec régularisation"""
    print("Création du réseau d'analyse d'échecs avec régularisation...")

    network = Neuron(
        loss='categorical_crossentropy',
        learning_rate=0.001,  # Réduit pour stabilité avec régularisation
        l2_lambda=0.0001      # Régularisation L2
    )

    # Architecture améliorée avec dropout pour éviter l'overfitting
    network.add_layer(768, 256, 'relu', dropout_rate=0.2)    # Entrée one-hot -> Couche cachée 1
    network.add_layer(256, 128, 'relu', dropout_rate=0.2)    # Couche cachée 1 -> Couche cachée 2
    network.add_layer(128, 64, 'relu', dropout_rate=0.1)     # Couche cachée 2 -> Couche cachée 3
    network.add_layer(64, 3, 'softmax', dropout_rate=0.0)    # Couche cachée 3 -> Sortie (pas de dropout sur sortie)

    print("Architecture du réseau avec régularisation:")
    print("  Entrée: 768 neurones (encodage one-hot FEN)")
    print("  Couche cachée 1: 256 neurones (ReLU + Dropout 20%)")
    print("  Couche cachée 2: 128 neurones (ReLU + Dropout 20%)")
    print("  Couche cachée 3: 64 neurones (ReLU + Dropout 10%)")
    print("  Sortie: 3 neurones (Softmax - Nothing/Check/Checkmate)")
    print("  Régularisation: L2 λ=0.0001")

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