#!/usr/bin/env python3
# chess_features_enhanced.py

import numpy as np

def extract_chess_features(fen):
    """Extract 50+ meaningful chess features from FEN"""
    board_part = fen.split()[0]
    
    features = []
    
    # 1. Piece counts (12)
    piece_map = {
        'P': 0, 'N': 1, 'B': 2, 'R': 3, 'Q': 4, 'K': 5,
        'p': 6, 'n': 7, 'b': 8, 'r': 9, 'q': 10, 'k': 11
    }
    
    piece_counts = np.zeros(12)
    white_pieces = []
    black_pieces = []
    
    # Parse board
    row = 0
    col = 0
    for char in board_part:
        if char == '/':
            row += 1
            col = 0
        elif char.isdigit():
            col += int(char)
        elif char in piece_map:
            piece_counts[piece_map[char]] += 1
            square_idx = row * 8 + col
            
            if char.isupper():  # White
                white_pieces.append((char, square_idx))
            else:  # Black
                black_pieces.append((char, square_idx))
            col += 1
    
    features.extend(piece_counts)
    
    # 2. Material balance (using chess piece values)
    piece_values = {
        'P': 1, 'N': 3, 'B': 3, 'R': 5, 'Q': 9, 'K': 0,
        'p': 1, 'n': 3, 'b': 3, 'r': 5, 'q': 9, 'k': 0
    }
    
    white_material = sum(piece_values[p] for p, _ in white_pieces)
    black_material = sum(piece_values[p] for p, _ in black_pieces)
    
    features.append(white_material)  # White material
    features.append(black_material)  # Black material
    features.append(white_material - black_material)  # Material difference
    features.append(len(white_pieces))  # White piece count
    features.append(len(black_pieces))  # Black piece count
    
    # 3. King positions and safety
    white_king_pos = None
    black_king_pos = None
    
    for piece, pos in white_pieces:
        if piece == 'K':
            white_king_pos = pos
            break
    
    for piece, pos in black_pieces:
        if piece == 'k':
            black_king_pos = pos
            break
    
    if white_king_pos is not None:
        wk_file = white_king_pos % 8
        wk_rank = white_king_pos // 8
        # Distance from center (e4/e5)
        wk_center_dist = abs(wk_file - 3.5) + abs(wk_rank - 3.5)
        features.extend([wk_file, wk_rank, wk_center_dist])
    else:
        features.extend([0, 0, 0])
    
    if black_king_pos is not None:
        bk_file = black_king_pos % 8
        bk_rank = black_king_pos // 8
        bk_center_dist = abs(bk_file - 3.5) + abs(bk_rank - 3.5)
        features.extend([bk_file, bk_rank, bk_center_dist])
    else:
        features.extend([0, 0, 0])
    
    # 4. Pawn structure
    white_pawn_files = [pos % 8 for p, pos in white_pieces if p == 'P']
    black_pawn_files = [pos % 8 for p, pos in black_pieces if p == 'p']
    
    # Pawn counts on each file (8 features)
    for file in range(8):
        features.append(white_pawn_files.count(file))
    
    for file in range(8):
        features.append(black_pawn_files.count(file))
    
    # 5. Piece mobility approximations
    # Rooks on open/semi-open files
    white_rooks = [pos % 8 for p, pos in white_pieces if p == 'R']
    black_rooks = [pos % 8 for p, pos in black_pieces if p == 'r']
    
    white_rooks_open = 0
    black_rooks_open = 0
    
    for file in range(8):
        file_has_white_pawn = file in white_pawn_files
        file_has_black_pawn = file in black_pawn_files
        
        if file in white_rooks and not file_has_black_pawn:
            white_rooks_open += 1
        
        if file in black_rooks and not file_has_white_pawn:
            black_rooks_open += 1
    
    features.append(white_rooks_open)
    features.append(black_rooks_open)
    
    # 6. Attack/defense approximations
    # Pieces close to enemy king
    if black_king_pos is not None:
        white_attackers = 0
        for piece, pos in white_pieces:
            if piece != 'K':  # Don't count king
                # Simple distance
                dist = abs(pos // 8 - black_king_pos // 8) + abs(pos % 8 - black_king_pos % 8)
                if dist <= 3:  # Pieces within 3 squares
                    white_attackers += 1
        features.append(white_attackers)
    else:
        features.append(0)
    
    if white_king_pos is not None:
        black_attackers = 0
        for piece, pos in black_pieces:
            if piece != 'k':
                dist = abs(pos // 8 - white_king_pos // 8) + abs(pos % 8 - white_king_pos % 8)
                if dist <= 3:
                    black_attackers += 1
        features.append(black_attackers)
    else:
        features.append(0)
    
    # 7. Endgame features
    total_pieces = len(white_pieces) + len(black_pieces)
    features.append(total_pieces)
    features.append(1 if total_pieces <= 10 else 0)  # Endgame indicator
    
    # Total: 12 + 5 + 6 + 16 + 2 + 2 + 2 = 45 features
    
    return np.array(features).reshape(-1, 1)

def test_features():
    """Test feature extraction"""
    print("Testing chess feature extraction...")
    
    fens = [
        "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1",  # Start
        "8/8/R2k4/4r1p1/8/5K2/5P2/8 b - - 7 59",  # Endgame
        "8/8/8/8/8/8/8/k1K5 w - - 0 1",  # Checkmate
    ]
    
    for fen in fens:
        features = extract_chess_features(fen)
        print(f"\nFEN: {fen[:40]}...")
        print(f"Feature vector shape: {features.shape}")
        print(f"Non-zero features: {np.sum(features != 0)}")
        print(f"Sample features: material diff={features[14]}, total pieces={features[43]}")

if __name__ == '__main__':
    test_features()