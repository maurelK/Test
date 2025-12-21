#!/usr/bin/env python3
import numpy as np

def debug_fen_encoding():
    # Test FEN from your dataset
    test_fens = [
        "8/8/R2k4/4r1p1/8/5K2/5P2/8 b - - 7 59",  # Check White
        "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1",  # Starting position
        "8/8/8/1p1p2p1/q1p5/8/K1k5/8 w - - 4 70",  # Checkmate Black
    ]
    
    for fen in test_fens:
        print(f"\nFEN: {fen}")
        
        board_part = fen.split()[0]
        print(f"Board part: {board_part}")
        
        piece_map = {
            'P': 0, 'N': 1, 'B': 2, 'R': 3, 'Q': 4, 'K': 5,
            'p': 6, 'n': 7, 'b': 8, 'r': 9, 'q': 10, 'k': 11
        }
        
        vector = np.zeros(768)
        square_idx = 0
        
        # Manual counting
        piece_counts = {}
        
        for char in board_part:
            if char.isdigit():
                empty_squares = int(char)
                print(f"  Empty squares: {empty_squares}")
                square_idx += empty_squares
            elif char == '/':
                continue
            elif char in piece_map:
                piece_counts[char] = piece_counts.get(char, 0) + 1
                channel = piece_map[char]
                vector[channel * 64 + square_idx] = 1
                square_idx += 1
        
        print(f"Total squares processed: {square_idx}")
        print(f"Piece counts: {dict(sorted(piece_counts.items()))}")
        print(f"Total pieces: {sum(piece_counts.values())}")
        
        non_zero = np.sum(vector != 0)
        print(f"Non-zero in vector: {non_zero}")
        print(f"Vector shape: {vector.shape}")
        
        # Check some positions
        print("Sample positions (channel, square):")
        for piece, channel in piece_map.items():
            start = channel * 64
            count = np.sum(vector[start:start+64] != 0)
            if count > 0:
                print(f"  {piece}: {count} at channel {channel}")

if __name__ == '__main__':
    debug_fen_encoding()