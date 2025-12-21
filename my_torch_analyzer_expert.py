#!/usr/bin/env python3
# my_torch_analyzer_expert.py

import sys
import argparse
import numpy as np
from my_torch.Neuron import Neuron
from chess_features_expert import extract_expert_features

# Normalization parameters (should be saved from training)
X_mean = None  # Load from training
X_std = None   # Load from training

def load_normalization():
    """Load normalization parameters"""
    # You should save these during training
    global X_mean, X_std
    # For now, we'll calculate from the dataset
    with open('balanced_dataset.txt', 'r') as f:
        features_list = []
        for line in f:
            line = line.strip()
            if not line:
                continue
            parts = line.split()
            if len(parts) < 7:
                continue
            if parts[-1] == 'Nothing':
                fen = ' '.join(parts[:-1])
            else:
                fen = ' '.join(parts[:-2])
            features = extract_expert_features(fen).flatten()
            features_list.append(features)
        
        features_array = np.array(features_list).T
        X_mean = np.mean(features_array, axis=1, keepdims=True)
        X_std = np.std(features_array, axis=1, keepdims=True) + 1e-10
    
    return X_mean, X_std

def predict_with_expert(network, fen):
    """Predict using expert features"""
    features = extract_expert_features(fen)
    # Normalize
    if X_mean is not None and X_std is not None:
        features = (features - X_mean) / X_std
    
    output = network.forward(features, training=False)
    pred_idx = np.argmax(output)
    
    classes = ['Nothing', 'Check White', 'Check Black', 
               'Checkmate White', 'Checkmate Black']
    
    # Determine color from FEN
    fen_parts = fen.split()
    if len(fen_parts) >= 2:
        turn = fen_parts[1]  # 'w' or 'b'
        if pred_idx == 1 or pred_idx == 3:  # Check or Checkmate
            color = 'White' if turn == 'w' else 'Black'
            if pred_idx == 1:
                return f"Check {color}"
            else:
                return f"Checkmate {color}"
        elif pred_idx == 2 or pred_idx == 4:
            color = 'Black' if turn == 'w' else 'White'
            if pred_idx == 2:
                return f"Check {color}"
            else:
                return f"Checkmate {color}"
    
    return ['Nothing', 'Check', 'Check', 'Checkmate', 'Checkmate'][pred_idx]

def main():
    # Load normalization
    load_normalization()
    
    parser = argparse.ArgumentParser(description='MY_TORCH Chess Analyzer with Expert Features')
    parser.add_argument('--predict', action='store_true', help='Predict mode')
    parser.add_argument('network_file', help='Network file')
    parser.add_argument('chess_file', help='Chess file with FEN strings')
    
    args = parser.parse_args()
    
    # Load network
    network = Neuron.load(args.network_file)
    
    # Predict
    with open(args.chess_file, 'r') as f:
        for line in f:
            line = line.strip()
            if not line:
                continue
            
            # Extract FEN (remove label if present)
            parts = line.split()
            known_labels = ['Nothing', 'Check', 'Checkmate', 'White', 'Black']
            
            fen_parts = []
            for part in parts:
                if part in known_labels:
                    break
                fen_parts.append(part)
            
            fen = ' '.join(fen_parts)
            
            # Predict
            prediction = predict_with_expert(network, fen)
            print(prediction)

if __name__ == '__main__':
    main()