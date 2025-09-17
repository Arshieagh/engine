#![allow(dead_code)]

use crate::{
  attacks::{self}, color_idx, position::{piece_kind_idx, Piece, PieceKind, Position}, square::Square, Color
};

#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub enum Castling {
  KingSide,
  QueenSide,
}

#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub struct Move {
  pub from: Square,
  pub to: Square,
  pub promotion: Option<PieceKind>, //None if not a promotion
  pub castle: Option<Castling>,
  pub ep: bool,
}

pub trait MoveGen {
  fn knight_moves(&self) -> Vec<Move>;
  fn bishop_moves(&self) -> Vec<Move>;
  fn rook_moves(&self) -> Vec<Move>;
  fn queen_moves(&self) -> Vec<Move>;
  /// Generate pseudo-legal pawn moves for the side to move.
  /// Captures use precomputed diagonal attack masks ∩ enemy occupancy; pushes require empty squares
  /// (single push; optional double from home rank); promotions are emitted as 4 separate moves.
  fn pawn_moves(&self) -> Vec<Move>;
  fn king_moves(&self) -> Vec<Move>;

  fn legal_moves(&self) -> Vec<Move>;
}

impl MoveGen for Position {
  fn knight_moves(&self) -> Vec<Move> {
      let mut moves = Vec::new();
      let color = color_idx(self.stm);
      let pk = piece_kind_idx(PieceKind::Knight);
      let knights = self.bb[color][pk];
      let mut copy = knights;
      let target_blocked = self.occ_side[color];

      while let Some(from_idx) = copy.pop_lsb() {
        let from = Square::from_index(from_idx).unwrap();
        let attacks = attacks::knight_attacks_tbl(from);
        let mut targets = attacks & !target_blocked;
        while let Some(to_idx) = targets.pop_lsb() {
          let to = Square::from_index(to_idx).unwrap();
          moves.push(Move { from, to, promotion: None, castle: None, ep: false });
        }
      }
      moves
  }
  fn bishop_moves(&self) -> Vec<Move> {
    let mut moves = Vec::new();
    let color = color_idx(self.stm);
    let pk = piece_kind_idx(PieceKind::Bishop);
    let bishops = self.bb[color][pk];
    let mut copy = bishops;
    let target_blocked = self.occ_side[color];

    while let Some(from_idx) = copy.pop_lsb() {
      let from = Square::from_index(from_idx).unwrap();
      let attacks = attacks::bishop_attacks(self.occ_all, from);
      let mut targets = attacks & !target_blocked;
      while let Some(to_idx) = targets.pop_lsb() {
        let to = Square::from_index(to_idx).unwrap();
        moves.push(Move { from, to, promotion: None, castle: None, ep: false });
      }
    }
    moves
  }

  fn rook_moves(&self) -> Vec<Move> {
      let mut moves = Vec::new();
      let color = color_idx(self.stm);
      let pk = piece_kind_idx(PieceKind::Rook);
      let rooks = self.bb[color][pk];
      let mut copy = rooks;
      let target_blocked = self.occ_side[color];

      while let Some(from_idx) = copy.pop_lsb() {
        let from = Square::from_index(from_idx).unwrap();
        let attacks = attacks::rook_attacks(self.occ_all, from);
        let mut targets = attacks & !target_blocked;
        while let Some(to_idx) = targets.pop_lsb() {
          let to = Square::from_index(to_idx).unwrap();
          moves.push(Move { from, to, promotion: None, castle: None, ep: false });
        }
      }
      moves
  }

  fn queen_moves(&self) -> Vec<Move> {
      let mut moves = Vec::new();
      let color = color_idx(self.stm);
      let pk = piece_kind_idx(PieceKind::Queen);
      let queens = self.bb[color][pk];
      let mut copy = queens;
      let target_blocked = self.occ_side[color];

      while let Some(from_idx) = copy.pop_lsb() {
        let from = Square::from_index(from_idx).unwrap();
        let attacks = attacks::queen_attacks(self.occ_all, from);
        let mut targets = attacks & !target_blocked;
        while let Some(to_idx) = targets.pop_lsb() {
          let to = Square::from_index(to_idx).unwrap();
          moves.push(Move { from, to, promotion: None, castle: None, ep: false });
        }
      }
      moves
  }

  fn pawn_moves(&self) -> Vec<Move> {
      let mut moves = Vec::new();
      let color = color_idx(self.stm);
      let pk = piece_kind_idx(PieceKind::Pawn);
      let pawns = self.bb[color][pk];
      let mut copy = pawns;
      let enemy_occ = self.enemy_occ();

      while let Some(from_idx) = copy.pop_lsb() {
        let from = Square::from_index(from_idx).unwrap();

        // Attacks
        let attacks = attacks::pawn_attacks(from, self.stm);
        let mut targets = attacks & enemy_occ;
        while let Some(to_idx) = targets.pop_lsb() {
          let to = Square::from_index(to_idx).unwrap();
          if to.rank() == 7 || to.rank() == 0 {
            moves.push(Move { from, to, promotion: Some(PieceKind::Queen), castle: None, ep:false});
            moves.push(Move { from, to, promotion: Some(PieceKind::Rook), castle: None, ep:false});
            moves.push(Move { from, to, promotion: Some(PieceKind::Bishop), castle: None, ep:false});
            moves.push(Move { from, to, promotion: Some(PieceKind::Knight), castle: None, ep:false});
          }
          else {
            moves.push(Move { from, to, promotion: None, castle: None, ep: false });
          }
        }

        if self.ep.is_some() && attacks.0 & 1u64 << self.ep.unwrap().index() != 0 {
          let to = self.ep.unwrap();
          moves.push(Move { from, to, promotion: None, castle: None, ep: true });
        }

        // Forward pushes (non-captures): depend on color and must be empty squares
        let from_rank = from.rank();
        if color == 0 {
          // White moves north (increasing index by 8)
          if from_rank < 7 {
            let one_step = Square::from_index(from_idx + 8).unwrap();
            if !self.occ_all.test(one_step) {
              // Promotion on push to rank 7
              if one_step.rank() == 7 {
                moves.push(Move { from, to: one_step, promotion: Some(PieceKind::Queen), castle: None, ep: false});
                moves.push(Move { from, to: one_step, promotion: Some(PieceKind::Rook), castle: None, ep: false});
                moves.push(Move { from, to: one_step, promotion: Some(PieceKind::Bishop), castle: None, ep: false});
                moves.push(Move { from, to: one_step, promotion: Some(PieceKind::Knight), castle: None, ep: false});
              } else {
                moves.push(Move { from, to: one_step, promotion: None, castle: None, ep: false });
                // Double push from rank 2 (index rank == 1) if both squares empty
                if from_rank == 1 {
                  let two_step = Square::from_index(from_idx + 16).unwrap();
                  if !self.occ_all.test(two_step) { 
                    moves.push(Move { from, to: two_step, promotion: None, castle: None, ep: false });
                  }
                }
              }
            }
          }
        } else {
          // Black moves south (decreasing index by 8)
          if from_rank > 0 {
            let one_step = Square::from_index(from_idx - 8).unwrap();
            if !self.occ_all.test(one_step) {
              // Promotion on push to rank 0
              if one_step.rank() == 0 {
                moves.push(Move { from, to: one_step, promotion: Some(PieceKind::Queen), castle: None, ep: false});
                moves.push(Move { from, to: one_step, promotion: Some(PieceKind::Rook), castle: None, ep: false});
                moves.push(Move { from, to: one_step, promotion: Some(PieceKind::Bishop), castle: None, ep: false});
                moves.push(Move { from, to: one_step, promotion: Some(PieceKind::Knight), castle: None, ep: false});
              } else {
                moves.push(Move { from, to: one_step, promotion: None, castle: None, ep: false });
                // Double push from rank 7 (index rank == 6) if both squares empty
                if from_rank == 6 {
                  let two_step = Square::from_index(from_idx - 16).unwrap();
                  if !self.occ_all.test(two_step) {
                    moves.push(Move { from, to: two_step, promotion: None, castle: None, ep: false });
                  }
                }
              }
            }
          }
        }
      }

      moves
  }

  fn king_moves(&self) -> Vec<Move> {
      let mut moves = Vec::new();
      let color = color_idx(self.stm);
      let target_blocked = self.occ_side[color];

      let from = self.king_square(self.stm).unwrap();
      let attacks = attacks::king_attacks_tbl(from);
      let mut targets = attacks & !target_blocked;
      while let Some(to_idx) = targets.pop_lsb() {
        let to = Square::from_index(to_idx).unwrap();
        moves.push(Move { from, to, promotion: None, castle: None, ep: false });
      }

      if self.castling & 0b11 != 0 && color == 0 {
        if from.index() != 4 {
          // White king not on e1 → cannot castle
          return moves;
        }
        let e1 = Square::from_index(4).unwrap();
        let opp = Color::Black;
        let rook = Piece { kind: PieceKind::Rook, color: Color::White };

        // White can castle kingside
        if self.castling & 0b01 != 0 {
          let f1 = Square::from_index(5).unwrap();
          let g1 = Square::from_index(6).unwrap();
          let h1 = Square::from_index(7).unwrap();

          if self.piece_at(h1) == Some(rook) &&
             !self.occ_all.test(f1) &&
             !self.occ_all.test(g1) && 
             !self.is_square_attacked_by(e1, opp) &&
             !self.is_square_attacked_by(f1, opp) &&
             !self.is_square_attacked_by(g1, opp) {
              moves.push(Move { from: e1, to: g1, promotion: None, castle: Some(Castling::KingSide), ep: false});
          }
        }
        // White can castle queenside
        if self.castling & 0b10 != 0 {
          let d1 = Square::from_index(3).unwrap();
          let c1 = Square::from_index(2).unwrap();
          let b1 = Square::from_index(1).unwrap();
          let a1 = Square::from_index(0).unwrap();

          if self.piece_at(a1) == Some(rook) &&
             !self.occ_all.test(d1) &&
             !self.occ_all.test(c1) && 
             !self.occ_all.test(b1) && 
             !self.is_square_attacked_by(e1, opp) &&
             !self.is_square_attacked_by(d1, opp) &&
             !self.is_square_attacked_by(c1, opp) {
              moves.push(Move { from: e1, to: c1, promotion: None, castle: Some(Castling::QueenSide), ep: false});
          }
        }
      }

      if self.castling & 0b1100 != 0 && color == 1 {
        if from.index() != 60 {
          // Black king not on e8 → cannot castle
          return moves;
        }
        let e8 = Square::from_index(60).unwrap();
        let opp = Color::White;
        let rook = Piece { kind: PieceKind::Rook, color: Color::Black };

        // Black can castle kingside
        if self.castling & 0b0100 != 0 {
          let f8 = Square::from_index(61).unwrap();
          let g8 = Square::from_index(62).unwrap();
          let h8 = Square::from_index(63).unwrap();

          if self.piece_at(h8) == Some(rook) &&
             !self.occ_all.test(f8) &&
             !self.occ_all.test(g8) && 
             !self.is_square_attacked_by(e8, opp) &&
             !self.is_square_attacked_by(f8, opp) &&
             !self.is_square_attacked_by(g8, opp) {
              moves.push(Move { from: e8, to: g8, promotion: None, castle: Some(Castling::KingSide), ep: false});
          }
        }
        // Black can castle queenside
        if self.castling & 0b1000 != 0 {
          let d8 = Square::from_index(59).unwrap();
          let c8 = Square::from_index(58).unwrap();
          let b8 = Square::from_index(57).unwrap();
          let a8 = Square::from_index(56).unwrap();

          if self.piece_at(a8) == Some(rook) &&
             !self.occ_all.test(d8) &&
             !self.occ_all.test(c8) && 
             !self.occ_all.test(b8) && 
             !self.is_square_attacked_by(e8, opp) &&
             !self.is_square_attacked_by(d8, opp) &&
             !self.is_square_attacked_by(c8, opp) {
              moves.push(Move { from: e8, to: c8, promotion: None, castle: Some(Castling::QueenSide), ep: false});
          }
        }
      }
      moves
  }

  fn legal_moves(&self) -> Vec<Move> {
    let mover = self.stm;
    let opp = Color::from_index(color_idx(mover) ^ 1).unwrap();
    let mut moves = Vec::new();

    let mut psuedo_legal_moves = Vec::new();
    psuedo_legal_moves.extend(self.pawn_moves());
    psuedo_legal_moves.extend(self.knight_moves());
    psuedo_legal_moves.extend(self.bishop_moves());
    psuedo_legal_moves.extend(self.rook_moves());
    psuedo_legal_moves.extend(self.queen_moves());
    let mut psuedo_legal_king_moves = Vec::new();
    psuedo_legal_king_moves.extend(self.king_moves());

    for mv in psuedo_legal_moves {
      let mut pos = *self;
      let king_sq = pos.king_square(mover).unwrap();
      let _ = pos.make_unchecked(&mv);
      if !pos.is_square_attacked_by(king_sq, opp) {
        moves.push(mv);
      }
    }

    for mv in psuedo_legal_king_moves {
      let mut pos = *self;
      let _ = pos.make_unchecked(&mv);
      let new_king_sq = mv.to;
      if !pos.is_square_attacked_by(new_king_sq, opp) {
        moves.push(mv);
      }
    }
    moves
  }
}

#[cfg(test)]
mod tests {
  use super::MoveGen; // bring the trait in-scope for method syntax
  use crate::fen::fen_parser;
  use crate::square::Square;

  #[test]
  fn knight_in_center_has_8_moves() {
    // Knight on d5 (index 35); empty board otherwise.
    let pos = fen_parser("8/8/8/3N4/8/8/8/8 w KQkq -").expect("FEN should parse");
    let moves = pos.knight_moves();
    assert_eq!(moves.len(), 8);
  }

  #[test]
  fn knight_blocked_by_friendly_piece_reduces_targets() {
    // White knight on d5, white pawn on f4 blocks one of the 8 targets
    let pos = fen_parser("8/8/8/3N4/5P2/8/8/8 w KQkq -").expect("FEN should parse");
    let moves = pos.knight_moves();
    assert_eq!(moves.len(), 7);
  }

  #[test]
  fn knight_can_capture_enemy_piece() {
    // White knight on d5, black pawn on f4 is capturable → still 8 total
    let pos = fen_parser("8/8/8/3N4/5p2/8/8/8 w KQkq -").expect("FEN should parse");
    let moves = pos.knight_moves();
    assert_eq!(moves.len(), 8);
  }

  #[test]
  fn rook_in_center_empty_board_14() {
    // Rook on d5, empty otherwise → 14 moves (N3 + S4 + E4 + W3)
    let pos = fen_parser("8/8/8/3R4/8/8/8/8 w KQkq -").expect("FEN should parse");
    let moves = pos.rook_moves();
    assert_eq!(moves.len(), 14);
  }

  #[test]
  fn rook_blocked_by_friendly_on_d4() {
    // White rook on d5, white pawn on d4 blocks the south ray entirely
    let pos = fen_parser("8/8/8/3R4/3P4/8/8/8 w KQkq -").expect("FEN should parse");
    let moves = pos.rook_moves();
    // From 14 → minus 4 south squares = 10
    assert_eq!(moves.len(), 10);
  }

  #[test]
  fn rook_can_capture_enemy_on_d4() {
    // White rook on d5, black pawn on d4: south ray includes d4 only
    let pos = fen_parser("8/8/8/3R4/3p4/8/8/8 w KQkq -").expect("FEN should parse");
    let moves = pos.rook_moves();
    // From 14 → south goes from 4 to 1 = 11
    assert_eq!(moves.len(), 11);
  }

  #[test]
  fn bishop_in_center_empty_board_13() {
    // Bishop on d5, empty otherwise → 13 moves
    let pos = fen_parser("8/8/8/3B4/8/8/8/8 w KQkq -").expect("FEN should parse");
    let moves = pos.bishop_moves();
    assert_eq!(moves.len(), 13);
  }

  #[test]
  fn bishop_blocked_by_friendly_on_f3() {
    // White bishop on d5, white pawn on f3 blocks down-right ray after e4
    let pos = fen_parser("8/8/8/3B4/8/5P2/8/8 w KQkq -").expect("FEN should parse");
    let moves = pos.bishop_moves();
    // SE ray shrinks from 4 to 1 → total 13 - 3 = 10
    assert_eq!(moves.len(), 10);
  }

  #[test]
  fn bishop_can_capture_enemy_on_f3() {
    // White bishop on d5, black pawn on f3: SE ray = e4,f3 (2 squares)
    let pos = fen_parser("8/8/8/3B4/8/5p2/8/8 w KQkq -").expect("FEN should parse");
    let moves = pos.bishop_moves();
    // SE from 4 to 2 → total 13 - 2 = 11
    assert_eq!(moves.len(), 11);
  }

  #[test]
  fn queen_in_center_empty_board_27() {
    // Queen on d5, empty otherwise → rook(14) + bishop(13) = 27
    let pos = fen_parser("8/8/8/3Q4/8/8/8/8 w KQkq -").expect("FEN should parse");
    let moves = pos.queen_moves();
    assert_eq!(moves.len(), 27);
  }

  #[test]
  fn queen_blocked_by_friendly_on_d4() {
    // White queen on d5, white pawn on d4 blocks south rook ray
    let pos = fen_parser("8/8/8/3Q4/3P4/8/8/8 w KQkq -").expect("FEN should parse");
    let moves = pos.queen_moves();
    // rook part 14→10, bishop part 13 unchanged → 23
    assert_eq!(moves.len(), 23);
  }

  #[test]
  fn queen_can_capture_enemy_on_d4() {
    // White queen on d5, black pawn on d4: south rook ray = d4 only
    let pos = fen_parser("8/8/8/3Q4/3p4/8/8/8 w KQkq -").expect("FEN should parse");
    let moves = pos.queen_moves();
    // rook part 14→11, bishop part 13 unchanged → 24
    assert_eq!(moves.len(), 24);
  }

  #[test]
  fn pawn_diagonal_into_empty_is_not_capture() {
    // White pawn on d4 with empty diagonals should produce 0 captures from diagonals
    let pos = fen_parser("8/8/8/8/3P4/8/8/8 w KQkq -").expect("FEN should parse");
    // Count only captures by placing enemies in diagonals later; here we assert pushes exist but no captures implied
    let moves = pos.pawn_moves();
    // Ensure none of the moves are diagonal without an enemy (i.e., all non-captures are vertical)
    // We simply assert at least one move exists (the push), and no promotions here.
    assert!(moves.iter().any(|m| m.to.index() == 35)); // d4 -> d5
  }

  #[test]
  fn pawn_single_push_blocked_and_allowed() {
    // Allowed: white pawn on d3 can push to d4
    let pos1 = fen_parser("8/8/8/8/8/3P4/8/8 w KQkq -").unwrap();
    assert!(pos1.pawn_moves().iter().any(|m| m.to.index() == 27));
    // Blocked: black pawn on d4 blocks white pawn from d3
    let pos2 = fen_parser("8/8/8/8/3p4/3P4/8/8 w KQkq -").unwrap();
    assert!(!pos2.pawn_moves().iter().any(|m| m.to.index() == 27));
  }

  #[test]
  fn pawn_double_push_rules() {
    // White double from d2 to d4 if d3 empty
    let pos = fen_parser("8/8/8/8/8/8/3P4/8 w KQkq -").unwrap();
    let ms = pos.pawn_moves();
    assert!(ms.iter().any(|m| m.to.index() == 27)); // d4
    // Blocked at d3 prevents double
    let pos_block = fen_parser("8/8/8/8/3p4/8/3P4/8 w KQkq -").unwrap();
    assert!(!pos_block.pawn_moves().iter().any(|m| m.to.index() == 27));
  }

  #[test]
  fn pawn_promotion_on_push_and_capture() {
    // Promotion on push: a7 -> a8
    let pos_push = fen_parser("8/P7/8/8/8/8/8/8 w KQkq -").unwrap();
    let ms_push = pos_push.pawn_moves();
    assert_eq!(ms_push.iter().filter(|m| m.to.index() == 56 && m.promotion.is_some()).count(), 4);
    // Promotion on capture: a7xb8
    let pos_cap = fen_parser("1k6/P7/8/8/8/8/8/8 w KQkq -").unwrap();
    let ms_cap = pos_cap.pawn_moves();
    // b8 index = 57
    assert_eq!(ms_cap.iter().filter(|m| m.to.index() == 57 && m.promotion.is_some()).count(), 4);
  }

  #[test]
  fn pawn_no_oob_on_edges() {
    // White pawn on a7; black pawn on a2 — generation should not panic
    let _ = fen_parser("8/P7/8/8/8/8/8/8 w KQkq -").unwrap().pawn_moves();
    let _ = fen_parser("8/8/8/8/8/8/P7/8 b KQkq -").unwrap(); // ensure parser ok for black stm
  }

  #[test]
  fn legal_moves_startpos_is_20() {
    let pos = fen_parser("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq -").unwrap();
    let ms = pos.legal_moves();
    assert_eq!(ms.len(), 20);
  }

  #[test]
  fn legal_moves_evasion_when_in_check() {
    // Position: Black rook on e2 gives check to white king on e1.
    // Legal moves for White should be: Kd1, Kf1, Kxe2 (3 moves), assuming no other black pieces protect e2.
    let pos = fen_parser("k7/8/8/8/8/8/4r3/4K3 w KQkq -").unwrap();
    let ms = pos.legal_moves();
    // Expect exactly three legal moves.
    assert_eq!(ms.len(), 3, "Expected 3 king evasions, got {}: {:?}", ms.len(), ms);
  }

  #[test]
  fn legal_moves_disallow_moving_pinned_piece() {
    // Black rook on e8 pins the white knight on e2 against the white king on e1.
    // White to move: any knight move from e2 would expose the king to check and must be filtered out.
    let pos = fen_parser("4r3/8/8/8/8/8/4N3/4K3 w KQkq -").unwrap();
    let ms = pos.legal_moves();
    // e2 index = 12
    assert!(ms.iter().all(|m| m.from.index() != 12), "Pinned knight should have no legal moves: {:?}", ms);
  }

  #[test]
  fn king_moves_white_has_both_castles_when_clear() {
    // Rooks on a1/h1, king on e1, empty path, no attacks on e1,f1,g1,d1,c1
    let pos = fen_parser("r3k2r/8/8/8/8/8/8/R3K2R w KQkq -").unwrap();
    let ms = pos.king_moves();
    let castles: Vec<_> = ms.iter().filter(|m| m.castle.is_some()).collect();
    assert_eq!(castles.len(), 2, "expected two castle moves for White, got {:?}", ms);
    // Ensure they are e1->g1 and e1->c1
    assert!(castles.iter().any(|m| m.from.index() == 4 && m.to.index() == 6));
    assert!(castles.iter().any(|m| m.from.index() == 4 && m.to.index() == 2));
    // Ensure no stray rook move is emitted as a 'castle'
    assert!(ms.iter().all(|m| m.from.index() != 7 && m.from.index() != 0));
  }

  #[test]
  fn king_moves_black_has_both_castles_when_clear() {
    // Same structure, Black to move
    let pos = fen_parser("r3k2r/8/8/8/8/8/8/R3K2R b KQkq -").unwrap();
    let ms = pos.king_moves();
    let castles: Vec<_> = ms.iter().filter(|m| m.castle.is_some()).collect();
    assert_eq!(castles.len(), 2, "expected two castle moves for Black, got {:?}", castles);
    // e8->g8 and e8->c8
    assert!(castles.iter().any(|m| m.from.index() == 60 && m.to.index() == 62));
    assert!(castles.iter().any(|m| m.from.index() == 60 && m.to.index() == 58));
    // No stray rook move
    assert!(ms.iter().all(|m| m.from.index() != 63 && m.from.index() != 56));
  }

  #[test]
  fn kingside_castle_blocked_by_attack_on_f1() {
    // Black bishop on a6 attacks f1 (a6-b5-c4-d3-e2-f1), so O-O is illegal for White.
    let pos = fen_parser("r3k2r/8/b7/8/8/8/8/R3K2R w KQkq -").unwrap();
    let ms = pos.king_moves();
    // No e1->g1 castle
    assert!(!ms.iter().any(|m| m.castle.is_some() && m.to.index() == 6));
  }

  #[test]
  fn queenside_castle_blocked_by_attack_on_d1() {
    // Black bishop on c2 attacks d1, so O-O-O is illegal for White.
    let pos = fen_parser("r3k2r/8/8/8/8/8/2b5/R3K2R w KQkq -").unwrap();
    let ms = pos.king_moves();
    // No e1->c1 castle
    assert!(!ms.iter().any(|m| m.castle.is_some() && m.to.index() == 2));
  }

  #[test]
  fn legal_moves_include_castles_when_clear() {
    let pos = fen_parser("r3k2r/8/8/8/8/8/8/R3K2R w KQkq -").unwrap();
    let ms = pos.legal_moves();
    let castle_cnt = ms.iter().filter(|m| m.castle.is_some()).count();
    assert_eq!(castle_cnt, 2);
  }

  #[test]
  #[ignore]
  fn debug_white_kingside_castle_predicates_in_kiwipete() {
     // for piece_at/test helpers if needed
    let pos = fen_parser("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq -").unwrap();

    // Squares
    let e1 = Square::from_index(4).unwrap();
    let f1 = Square::from_index(5).unwrap();
    let g1 = Square::from_index(6).unwrap();
    let h1 = Square::from_index(7).unwrap();

    // Expectations
    let rook = crate::position::Piece { kind: crate::position::PieceKind::Rook, color: crate::Color::White };
    let opp = crate::Color::Black;

    let right = pos.castling & 0b0001 != 0;
    let king_on_e1 = pos.king_square(crate::Color::White) == Some(e1);
    let rook_on_h1 = pos.piece_at(h1) == Some(rook);
    let f1_empty = !pos.occ_all.test(f1);
    let g1_empty = !pos.occ_all.test(g1);
    let e1_safe = !pos.is_square_attacked_by(e1, opp);
    let f1_safe = !pos.is_square_attacked_by(f1, opp);
    let g1_safe = !pos.is_square_attacked_by(g1, opp);

    eprintln!("WK right: {}", right);
    eprintln!("king on e1: {}", king_on_e1);
    eprintln!("rook on h1: {}", rook_on_h1);
    eprintln!("f1 empty: {}  g1 empty: {}", f1_empty, g1_empty);
    eprintln!("e1 safe: {}  f1 safe: {}  g1 safe: {}", e1_safe, f1_safe, g1_safe);

    // Final predicate mirrors king_moves() logic for O-O
    let allow = right && king_on_e1 && rook_on_h1 && f1_empty && g1_empty && e1_safe && f1_safe && g1_safe;
    eprintln!("allow O-O? {}", allow);
  }
}