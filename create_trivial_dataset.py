#!/usr/bin/env python3
# create_trivial_dataset.py

import numpy as np

def create_trivial_chess_rules():
    """Create a dataset based on SIMPLE rules a NN could learn"""
    
    rules = [
        # Rule 1: If white king alone -> Checkmate (for black)
        ("K", "Checkmate Black"),
        
        # Rule 2: If black king alone -> Checkmate (for white)  
        ("k", "Checkmate White"),
        
        # Rule 3: King + Queen vs King -> Checkmate
        ("KQk", "Checkmate White"),
        ("Kkq", "Checkmate Black"),
        
        # Rule 4: Two rooks vs king -> Checkmate
        ("KRRk", "Checkmate White"),
        ("Kkrr", "Checkmate Black"),
        
        # Rule 5: King vs King -> Nothing
        ("Kk", "Nothing"),
        
        # Rule 6: Pawn present -> Usually Nothing (but could be Check)
        ("Kkp", "Nothing"),
        ("KkP", "Nothing"),
    ]
    
    samples = []
    
    for pieces, label in rules:
        # Create multiple variations of each rule
        for _ in range(50):
            # Create a random board with these pieces
            board = ["."] * 64
            
            # Place pieces randomly
            positions = np.random.choice(64, len(pieces), replace=False)
            for piece, pos in zip(pieces, positions):
                board[pos] = piece
            
            # Convert to FEN
            fen_rows = []
            for i in range(8):
                row = board[i*8:(i+1)*8]
                fen_row = ""
                empty_count = 0
                
                for sq in row:
                    if sq == ".":
                        empty_count += 1
                    else:
                        if empty_count > 0:
                            fen_row += str(empty_count)
                            empty_count = 0
                        fen_row += sq
                
                if empty_count > 0:
                    fen_row += str(empty_count)
                
                fen_rows.append(fen_row)
            
            fen_board = "/".join(fen_rows)
            fen = f"{fen_board} w - - 0 1"
            
            samples.append((fen, label))
    
    # Save
    with open('trivial_dataset.txt', 'w') as f:
        for fen, label in samples:
            f.write(f"{fen} {label}\n")
    
    print(f"Created {len(samples)} trivial samples")
    print("Rules are based on piece counts, not actual chess logic")
    
    return samples

if __name__ == '__main__':
    create_trivial_chess_rules()